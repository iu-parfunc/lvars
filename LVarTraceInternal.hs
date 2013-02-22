{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
{-# LANGUAGE RankNTypes, NamedFieldPuns, BangPatterns,
             ExistentialQuantification, CPP
	     #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

-- | This (experimental) module generalizes the Par monad to allow arbitrary LVars
-- (lattice variables) not just IVars.
-- 
-- This module exposes the internals of the @Par@ monad so that you
-- can build your own scheduler or other extensions.  Do not use this
-- module for purposes other than extending the @Par@ monad with new
-- functionality.

module LVarTraceInternal where
 --       (
 --   Trace(..), Sched(..), Par(..),
 --   IVar(..), IVarContents(..),
 --   sched,
 --   runPar, runParIO, runParAsync,
 --   -- runParAsyncHelper,
 --   new, newFull, newFull_, get, put_, put,
 --   pollIVar, yield,
 -- ) where


import Control.Monad hiding (mapM, sequence, join)
import Prelude hiding (mapM, sequence, head,tail)
import Data.IORef
import qualified Data.Map as M
import System.IO.Unsafe
import Control.Concurrent hiding (yield)
import GHC.Conc hiding (yield)
import Control.DeepSeq
import Control.Applicative

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


-- | An LVar consists of a piece of mutable state, and a list of polling
-- functions that produce continuations when they are successfull.
--
-- This implementation cannot provide scalable LVars (e.g. a concurrent hashmap),
-- rather accesses to a single LVar will contend.  But even if operations on an LVar
-- are serialized, they cannot all be a single "atomicModifyIORef", because atomic
-- modifies must be pure functions whereas the LVar polling functions are in the IO
-- monad.
data LVar a = LVar {
  lvstate :: a,
  blocked :: {-# UNPACK #-} !(IORef (M.Map UID Poller))
}

-- A poller can only be removed from the Map when it is woken.
data Poller = Poller {
   poll  :: IO (Maybe Trace),
   woken :: {-# UNPACK #-}! (IORef Bool)
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
-- IVars implemented on top of LVars:

type IVar a = LVar (IORef (IVarContents a))
data IVarContents a = Full a | Empty -- | Blocked [a -> Trace]

new :: Par (IVar a)
new = newLV (newIORef Empty)

-- | read the value in a @IVar@.  The 'get' can only return when the
-- value has been written by a prior or parallel @put@ to the same
-- @IVar@.
get :: IVar a -> Par a
get iv@(LVar ref waitls) = getLV iv poll
 where
   poll = do contents <- readIORef ref
             case contents of
               Full x -> return$ Just x
               Empty  -> return Nothing

-- | put a value into a @IVar@.  Multiple 'put's to the same @IVar@
-- are not allowed, and result in a runtime error.
put_ :: IVar a -> a -> Par ()
put_ iv elt = putLV iv putter
 where
   putter ref =
     atomicModifyIORef ref $ \ x ->
        case x of
          Empty  -> (Full elt, ())
          Full _ -> error "multiple puts to an IVar"

-- ---------------------------------------------------------------------------
-- Generic scheduler with LVars:
-- ---------------------------------------------------------------------------

-- | Trying this using only parametric polymorphism:
data Trace =
             forall a b . Get (LVar a) (IO (Maybe b)) (b -> Trace)
           | forall a . Put (LVar a) (a -> IO ()) Trace
           | forall a . New (IO a) (LVar a -> Trace)
           | Fork Trace Trace
           | Done
           | DoIO (IO ()) Trace
           | Yield Trace


-- | The main scheduler loop.
sched :: Bool -> Sched -> Trace -> IO ()
sched _doSync queue t = loop t
 where 
  loop t = case t of
    New io fn -> do
      x  <- io
      ls <- newIORef M.empty
      loop (fn (LVar x ls))

    Get (LVar v waitls) poll cont -> do
      e <- poll
      case e of         
         Just a  -> loop (cont a) -- Return straight away.
         Nothing -> do -- Register on the waitlist:
           uid <- getUID
           flag <- newIORef False

           -- FIXME: data-race: what if a putter runs to completion right now.
           -- Reader write lock?
           
           let retry = Poller (fmap cont <$> poll) flag
           atomicModifyIORef waitls $ \mp -> (M.insert uid retry mp, ())

           -- We must SELF POLL here to make sure there wasn't a race with a runner.
           

    Put (LVar v waitls) mutator tr -> do
      -- Here we follow an unfortunately expensive protocol.
      mutator v

      pollers <- readIORef waitls
      -- Innefficiency #1: we must leave the pollers in the list, where they may be
      -- redundantly evaluated:


      -- Now we TRY to scan the waitlist, but someone else may mutate in the middle.
      -- Because changes must be monotonic, this never allows false positives in the
      -- polling functions, but it may allow false negatives, and we may miss
      -- something which should unblock.
      
      -- Here we have a problem... we can't atomically run polling functions in the
      -- waitlist..  They're in IO.




      -- FIXME

      -- cs <- atomicModifyIORef waitls $ \ls -> case e of
      --          Empty    -> (Full a, [])
      --          Full _   -> error "multiple put"
      --          Blocked cs -> (Full a, cs)
      -- mapM_ (pushWork queue. ($a)) cs
      loop t

    Fork child parent -> do
         pushWork queue child
         loop parent
    Done ->
         if _doSync
	 then reschedule queue
         -- We could fork an extra thread here to keep numCapabilities workers
         -- even when the main thread returns to the runPar caller...
         else do putStrLn " [par] Forking replacement thread..\n"
                 forkIO (reschedule queue); return ()
         -- But even if we don't we are not orphaning any work in this
         -- threads work-queue because it can be stolen by other threads.
         --	 else return ()

    DoIO io t -> io >> loop t

    Yield parent -> do 
        -- Go to the end of the worklist:
        let Sched { workpool } = queue
        -- TODO: Perhaps consider Data.Seq here.
	-- This would also be a chance to steal and work from opposite ends of the queue.
        atomicModifyIORef workpool $ \ts -> (ts++[parent], ())
	reschedule queue

-- | Process the next item on the work queue or, failing that, go into
--   work-stealing mode.
reschedule :: Sched -> IO ()
reschedule _ = return ()
reschedule queue@Sched{ workpool } = do
  e <- atomicModifyIORef workpool $ \ts ->
         case ts of
           []      -> ([], Nothing)
           (t:ts') -> (ts', Just t)
  case e of
    Nothing -> steal queue
    Just t  -> sched True queue t

-- RRN: Note -- NOT doing random work stealing breaks the traditional
-- Cilk time/space bounds if one is running strictly nested (series
-- parallel) programs.

-- | Attempt to steal work or, failing that, give up and go idle.
steal :: Sched -> IO ()
steal _ = return ()
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
              sched True q t
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
runPar_internal :: Bool -> Par a -> IO a
runPar_internal _doSync x = do
   workpools <- replicateM numCapabilities $ newIORef []
   idle <- newIORef []
   let states = [ Sched { no=x, workpool=wp, idle, scheds=states }
                | (x,wp) <- zip [0..] workpools ]

#if __GLASGOW_HASKELL__ >= 701 /* 20110301 */
    --
    -- We create a thread on each CPU with forkOnIO.  The CPU on which
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

   m <- newEmptyMVar
   forM_ (zip [0..] states) $ \(cpu,state) ->
        forkOnIO cpu $
          if (cpu /= main_cpu)
             then reschedule state
             else sched _doSync state $ runCont (do x' <- x; liftIO (putMVar m x')) (const Done)
   takeMVar m


runPar :: Par a -> a
runPar = unsafePerformIO . runPar_internal True

-- | A version that avoids an internal `unsafePerformIO` for calling
--   contexts that are already in the `IO` monad.
runParIO :: Par a -> IO a
runParIO = runPar_internal True

-- | An asynchronous version in which the main thread of control in a
-- Par computation can return while forked computations still run in
-- the background.  
runParAsync :: Par a -> a
runParAsync = unsafePerformIO . runPar_internal False

-- -----------------------------------------------------------------------------

-- | Internal operation.  Creates a new @LVar@ with an initial value
newLV :: IO lv -> Par (LVar lv)
newLV init = Par $ New init

-- | Internal operation.  Test if the LVar satisfies the given threshold.
getLV :: LVar a -> (IO (Maybe b)) -> Par b
getLV lv poll = Par $ Get lv poll

-- | Internal operation.  Modify the LVar.  Had better be monotonic.
putLV :: LVar a -> (a -> IO ()) -> Par ()
putLV lv fn = Par $ \c -> Put lv fn (c ())

liftIO :: IO () -> Par ()
liftIO io = Par $ \c -> DoIO io (c ())

