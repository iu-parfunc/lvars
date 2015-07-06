{-# LANGUAGE RankNTypes #-} 
{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns, BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

-- | This (experimental) module generalizes the Par monad to allow
-- arbitrary LVars (lattice variables), not just IVars.
-- 
-- This module exposes the internals of the @Par@ monad so that you
-- can build your own scheduler or other extensions.  Do not use this
-- module for purposes other than extending the @Par@ monad with new
-- functionality.

module LVarTracePure
  (
    -- * LVar interface (for library writers):
   runParIO, fork, LVar(..), newLV, getLV, putLV, liftIO,
   Par(),
   Trace(..), runCont, consumeLV, newLVWithCallback,
   IVarContents(..),
   LVarContents(..),

   -- * Example use case: Basic IVar ops.
   runPar, IVar(), new, put, put_, get, spawn, spawn_, spawnP,
   
   -- * DEBUGGING only:
   unsafePeekLV
  ) where

import           Control.Monad hiding (sequence, join)
import           Control.Concurrent hiding (yield)
import           Control.DeepSeq
import           Control.Applicative
import           Data.IORef
import           Data.Maybe (catMaybes)
import qualified Data.Map as M
import qualified Data.Set as S
import           GHC.Conc hiding (yield)
import           System.IO.Unsafe (unsafePerformIO)
import           Prelude  hiding (mapM, sequence, head, tail)
import           Debug.Trace (trace)
import           System.Mem.StableName (makeStableName, hashStableName)

import qualified Control.Monad.Par.Class as PC

import Control.Concurrent.Async (asyncOn, withAsync, withAsyncOn, wait, mapConcurrently)
-- import Common (forkWithExceptions)

-- From 'lattices' package:  Classes for join semi-lattices, top, bottom:
import Algebra.Lattice (JoinSemiLattice(..))

------------------------------------------------------------------------------
-- IVars implemented on top of LVars:
------------------------------------------------------------------------------

-- TODO: newtype and hide the constructor:
newtype IVar a = IVar (LVar (IVarContents a))

newtype IVarContents a = IVC (Maybe a)
fromIVarContents :: IVarContents a -> Maybe a
fromIVarContents (IVC x) = x

new :: Par (IVar a)
new = IVar <$> newLV (IVC Nothing)

-- | read the value in a @IVar@.  The 'get' can only return when the
-- value has been written by a prior or parallel @put@ to the same
-- @IVar@.
get :: IVar a -> Par a
get (IVar lv) = getLV lv fromIVarContents

instance JoinSemiLattice (IVarContents a) where 
  join a (IVC Nothing) = a
  join (IVC Nothing) b = b
  join (IVC (Just _))
       (IVC (Just _)) = error "Multiple puts to an IVar!"

-- | put a value into a @IVar@.  Multiple 'put's to the same @IVar@
-- are not allowed, and result in a runtime error.
-- put_ :: Eq a => IVar a -> a -> Par ()
put_ :: IVar a -> a -> Par ()
put_ (IVar iv) elt = putLV iv (IVC (Just elt))

instance NFData (IVarContents a) where
  rnf (IVC (Just _)) = ()
  rnf (IVC Nothing)  = ()

spawn :: NFData a => Par a -> Par (IVar a)
spawn p  = do r <- new;  fork (p >>= put r);   return r
              
spawn_ :: Par a -> Par (IVar a)
spawn_ p = do r <- new;  fork (p >>= put_ r);  return r

spawnP :: NFData a => a -> Par (IVar a)
spawnP a = spawn (return a)

put :: NFData a => IVar a -> a -> Par ()
put v a = deepseq a (put_ v a)

instance PC.ParFuture IVar Par where
  spawn_ = spawn_
  get = get

instance PC.ParIVar IVar Par where
  fork = fork  
  put_ = put_
  new = new

------------------------------------------------------------------------------
-- Underlying LVar representation:
------------------------------------------------------------------------------

-- | An LVar consists of: a box containing a purely functional data
-- structure, and an optional callback function, triggered after every
-- write to the LVar, that takes the just-written value as its
-- argument.
-- 
-- This implementation cannot provide scalable LVars (e.g., a
-- concurrent hashmap); rather, accesses to a single LVar will
-- contend.
data LVar a = LVar {
  -- TODO: consider MutVar# 
  lvstate :: {-# UNPACK #-} !(IORef (LVarContents a)),
  callback :: Maybe (a -> a -> Trace)
}

data LVarContents a = LVarContents {
    current :: a,
    blocked :: [a -> Maybe Trace]
 }

-- Return the old value.  Could replace with a true atomic op.
atomicIncr :: IORef Int -> IO Int
atomicIncr cntr = atomicModifyIORef cntr (\c -> (c+1,c))

type UID = Int

uidCntr :: IORef UID
uidCntr = unsafePerformIO (newIORef 0)

getUID :: IO UID
getUID =  atomicIncr uidCntr

------------------------------------------------------------------------------
-- Generic scheduler with LVars:
------------------------------------------------------------------------------

newtype Par a = Par {
    runCont :: (a -> Trace) -> Trace
}

instance Functor Par where
    fmap f m = Par $ \c -> runCont m (c . f)

instance Monad Par where
    return a = Par ($ a)
    m >>= k  = Par $ \c -> runCont m $ \a -> runCont (k a) c

instance Applicative Par where
   (<*>) = ap
   pure  = return

-- | Trying this using only parametric polymorphism:
data Trace =
             forall a b . Get (LVar a) (a -> Maybe b) (b -> Trace)
           | forall a . (JoinSemiLattice a, NFData a) => Put (LVar a) a Trace
           | forall a . New a (LVar a -> Trace)
           | Fork Trace Trace
           | Done
           | DoIO (IO ()) Trace
           | Yield Trace

           -- Destructively consume the value (limited
           -- nondeterminism):
           | forall a . Consume (LVar a) (a -> Trace)

           -- For debugging:
           | forall a . Peek (LVar a) (a -> Trace)

           -- The callback (unsafely) is scheduled when there is ANY
           -- change.  It does NOT get a snapshot of the (continuously
           -- mutating) state, just the right to read whatever it can.
           | forall a . NewWithCallBack a (a -> a -> Trace) (LVar a -> Trace)

-- | The main scheduler loop.
sched :: Sched -> Trace -> IO ()
sched queue t = loop t
 where
  loop origt = case origt of
    New init cont -> do
      ref <- newIORef$ LVarContents init []
      loop (cont (LVar ref Nothing))

    NewWithCallBack init cb cont -> do
      ref <- newIORef$ LVarContents init []
      loop (cont$ LVar ref (Just cb))

    Get (LVar ref _) thresh cont -> do
      -- Tradeoff: we could do a plain read before the
      -- atomicModifyIORef.  But that would require evaluating the
      -- threshold function TWICE if we need to block.  (Which is
      -- potentially more expensive than in the plain IVar case.)
      -- e <- readIORef ref
      let thisCB x =
--            trace ("... LVar blocked, thresh attempted "++show(hashStableName$ unsafePerformIO$ makeStableName x))
            fmap cont $ thresh x
      r <- atomicModifyIORef ref $ \ st@(LVarContents a ls) ->
        case thresh a of
          Just b  -> -- trace ("... LVar get, thresh passed ")
                     (st, loop (cont b))
          Nothing -> (LVarContents a (thisCB:ls), reschedule queue)
      r

    Consume (LVar ref _) cont -> do
      -- HACK!  We know nothing about the type of state.  But we CAN
      -- destroy it to prevent any future access:
      a <- atomicModifyIORef ref (\(LVarContents a _) ->
                                   (error "attempt to touch LVar after Consume operation!", a))
      loop (cont a)

    Peek (LVar ref _) cont -> do
      LVarContents {current} <- readIORef ref     
      loop (cont current)

    Put (LVar ref cb) new tr  -> do
      cs <- atomicModifyIORef ref $ \e -> case e of
              LVarContents a ls ->
                let new' = join a new
                    (ls',woken) = loop ls [] []
                    loop [] f w = (f,w)
                    loop (hd:tl) f w =
                      case hd new' of
                        Just trc -> loop tl f (trc:w)
                        Nothing  -> loop tl (hd:f) w
                    -- Callbacks invoked: 
                    woken' = case cb of
                              Nothing -> woken
                              Just fn -> fn a new' : woken
                in 
                deepseq new' (LVarContents new' ls', woken')
      mapM_ (pushWork queue) cs
      loop tr              

    Fork child parent -> do
         pushWork queue parent -- "Work-first" policy.
         loop child
         -- pushWork queue child -- "Help-first" policy.  Generally bad.
         -- loop parent

    Done -> reschedule queue

    DoIO io t -> io >> loop t

    Yield parent -> do 
        -- Go to the end of the worklist:
        let Sched { workpool } = queue
        -- TODO: Perhaps consider Data.Seq here.  This would also be a
	-- chance to steal and work from opposite ends of the queue.
        atomicModifyIORef workpool $ \ts -> (ts++[parent], ())
	reschedule queue

-- | Process the next item on the work queue or, failing that, go into
-- work-stealing mode.
reschedule :: Sched -> IO ()
reschedule queue@Sched{ workpool } = do
  e <- atomicModifyIORef workpool $ \ts ->
         case ts of
           []      -> ([], Nothing)
           (t:ts') -> (ts', Just t)
  case e of
    Nothing -> steal queue
    Just t  -> sched queue t

-- RRN: Note -- NOT doing random work stealing breaks the traditional
-- Cilk time/space bounds if one is running strictly nested (series
-- parallel) programs.

-- | Attempt to steal work or, failing that, give up and go idle.
steal :: Sched -> IO ()
steal q@Sched{ idle, scheds, no=my_no } = do
  -- printf "cpu %d stealing\n" my_no
  go scheds
  where
    go [] = do m <- newEmptyMVar
               r <- atomicModifyIORef idle $ \is -> (m:is, is)
               if length r == numCapabilities - 1
                  then do
                     -- printf "cpu %d initiating shutdown\n" my_no
                     mapM_ (\m -> putMVar m True) r
                  else do
                    done <- takeMVar m
                    if done
                       then do
                         -- printf "cpu %d shutting down\n" my_no
                         return ()
                       else do
                         -- printf "cpu %d woken up\n" my_no
                         go scheds
    go (x:xs)
      | no x == my_no = go xs
      | otherwise     = do
         r <- atomicModifyIORef (workpool x) $ \ ts ->
                 case ts of
                    []     -> ([], Nothing)
                    (x:xs) -> (xs, Just x)
         case r of
           Just t  -> do
              -- printf "cpu %d got work from cpu %d\n" my_no (no x)
              sched q t
           Nothing -> go xs

-- | If any worker is idle, wake one up and give it work to do.
pushWork :: Sched -> Trace -> IO ()
pushWork Sched { workpool, idle } t = do
  atomicModifyIORef workpool $ \ts -> (t:ts, ())
  idles <- readIORef idle
  when (not (null idles)) $ do
    r <- atomicModifyIORef idle (\is -> case is of
                                          [] -> ([], return ())
                                          (i:is) -> (is, putMVar i False))
    r -- wake one up

data Sched = Sched
    { no       :: {-# UNPACK #-} !Int,
      workpool :: IORef [Trace],
      idle     :: IORef [MVar Bool],
      scheds   :: [Sched] -- Global list of all per-thread workers.
    }

-- Forcing evaluation of a LVar is fruitless.
instance NFData (LVar a) where
  rnf _ = ()

{-# INLINE runPar_internal #-}
runPar_internal :: Par a -> IO a
runPar_internal usercomp = do
   let numWorkers = numCapabilities
   workpools <- replicateM numWorkers $ newIORef []
   idle <- newIORef []
   let states = [ Sched { no=x, workpool=wp, idle, scheds=states }
                | (x,wp) <- zip [0..] workpools ]

#if __GLASGOW_HASKELL__ >= 701 /* 20110301 */
    --
    -- We create a thread on each CPU with forkOn.  The CPU on which
    -- the current thread is running will host the main thread; the
    -- other CPUs will host worker threads.
    --
    -- Note: GHC 7.1.20110301 is required for this to work, because that
    -- is when threadCapability was added.
    --
   (main_cpu, _) <- threadCapability =<< myThreadId
#else
    --
    -- Lacking threadCapability, we always pick CPU #0 to run the main
    -- thread.  If the current thread is not running on CPU #0, this
    -- will require some data to be shipped over the memory bus, and
    -- hence will be slightly slower than the version above.
    --
   let main_cpu = 0
#endif
   answerMV <- newEmptyMVar
   let runWorker state = do 
         let cpu = no state
	 if (cpu /= main_cpu)
	    then reschedule state
	    else sched state $
		   runCont (do x' <- usercomp
			       liftIO (putMVar answerMV x'))
			   (const Done)

   -- Here we want a traditional, fork-join parallel loop with proper exception handling:
   let loop [] asyncs = mapM_ wait asyncs
       loop (state:tl) asyncs = 
--         withAsync (runWorker state)
         withAsyncOn (no state) (runWorker state)
                     (\a -> loop tl (a:asyncs))

----------------------------------------
-- (1) There is a BUG in 'loop' presently:
--    "thread blocked indefinitely in an STM transaction"
--   loop states []
----------------------------------------
-- (2) This has the same problem as 'loop':
--   ls <- mapM (\ x -> asyncOn (no x) (runWorker x)) states
--   mapM_ wait ls
----------------------------------------
-- (3) Using this FOR NOW, but it does NOT pin to the right processors:
   mapConcurrently runWorker states
----------------------------------------
   -- Now that child threads are done, it's safe for the main thread
   -- to call it quits.
   takeMVar answerMV

runPar :: Par a -> a
runPar = unsafePerformIO . runPar_internal 

-- | A version that avoids an internal `unsafePerformIO` for calling
--   contexts that are already in the `IO` monad.
runParIO :: Par a -> IO a
runParIO = runPar_internal 

--------------------------------------------------------------------------------
-- Basic stuff:

-- Not in 6.12: {- INLINABLE fork -}
{-# INLINE fork #-}
fork :: Par () -> Par ()
fork p = Par $ \c -> Fork (runCont p (\_ -> Done)) (c ())

--------------------------------------------------------------------------------

-- | Internal operation.  Creates a new @LVar@ with an initial value
newLV :: lv -> Par (LVar lv)
newLV init = Par $ New init

-- | In this internal version, the callback gets to see the OLD and
-- the NEW version, respectively.
newLVWithCallback :: lv -> (lv -> lv -> Trace) -> Par (LVar lv)
newLVWithCallback st cb = Par $ NewWithCallBack st cb
                    
-- | Internal operation.  Test if the LVar satisfies the given
-- threshold.
getLV :: LVar a -> (a -> Maybe b) -> Par b
getLV lv test = Par $ Get lv test

-- | Internal operation.  Modify the LVar.  Had better be monotonic.
putLV :: (JoinSemiLattice a, NFData a) => LVar a -> a -> Par ()
putLV lv st = Par $ \c -> Put lv st (c ())

-- | Internal operation. Destructively consume the LVar, yielding
-- access to its precise state.
consumeLV :: LVar a -> Par a
consumeLV = Par . Consume 

unsafePeekLV :: LVar a -> Par a
unsafePeekLV = Par . Peek

liftIO :: IO () -> Par ()
liftIO io = Par $ \c -> DoIO io (c ())
