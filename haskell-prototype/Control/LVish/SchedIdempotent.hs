{-# LANGUAGE RankNTypes #-} 
{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns, BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

-- | This (experimental) module generalizes the Par monad to allow
-- arbitrary LVars (lattice variables), not just IVars.

module Control.LVish.SchedIdempotent
  (LVar(), state, HandlerPool(), newLV, getLV, putLV, freezeLV, freezeLVAfter,
   newPool, addHandler, quiesce, fork, liftIO, yield, Par(),
   runParIO
  ) where

import           Control.Monad hiding (sequence, join)
import           Control.Concurrent hiding (yield)
import qualified Control.Exception as E
import           Control.DeepSeq
import           Control.Applicative
import           Control.Concurrent.Async as Async
import           Data.IORef
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Concurrent.Counter as C
import qualified Data.Concurrent.Bag as B
import           GHC.Conc hiding (yield)
import           System.IO.Unsafe (unsafePerformIO)
import           Prelude  hiding (mapM, sequence, head, tail)

import           Old.Common (forkWithExceptions)

import qualified Control.LVish.SchedIdempotentInternal as Sched
  
------------------------------------------------------------------------------
-- LVar and Par monad representation
------------------------------------------------------------------------------

-- | LVars are parameterized by two types:
-- 
--     * The first, @a@, characterizes the "state" of the LVar (i.e. the lattice
--     value), and should be a concurrently mutable data type.  That means, in
--     particular, that only a /transient snapshot/ of the lattice value can be
--     obtained in general.  But the information in such a snapshot is always a
--     lower bound on the current value of the LVar.
--
--     * The second, @d@, characterizes the "delta" associated with a @putLV@
--     operation (i.e. the actual change, if any, to the LVar's lattice value).
--     In many cases such deltas allow far more efficient communication between
--     @putLV@s and blocked @getLV@s or handlers.  It is crucial, however, that
--     the behavior of a @get@ or handler does not depend on the /particular/
--     choice of @putLV@ operations (and hence deltas) that moved the LVar over
--     the threshold.  For simple data structures, the delta may just be the
--     entire LVar state, but for e.g. collection data structures, delta will
--     generally represent a single insertion.
data LVar a d = LVar {
  state  :: a,                -- the current, "global" state of the LVar
  status :: IORef (Status d), -- is the LVar active or frozen?  
  name   :: LVarID            -- a unique identifier for this LVar
}

type LVarID = IORef ()
newLVID = newIORef ()

-- a global ID that is *not* the name of any LVar.  Makes it possible to
-- represent Maybe (LVarID) with the type LVarID -- i.e., without any allocation.
noName :: LVarID
noName = unsafePerformIO $ newLVID

-- The frozen bit of an LVar is tied together with the bag of waiting listeners,
-- which allows the entire bag to become garbage immediately after freezing.
-- (Note, however, that outstanding @put@s that occurred just before freezing
-- may still reference the bag, which is necessary to ensure that all listeners
-- are informed of the @put@ prior to freezing.)
data Status d 
  = Frozen                       -- further changes to the state are forbidden
  | Active (B.Bag (Listener d))  -- bag of blocked threshold reads and handlers

-- A listener for an LVar is informed of each change to the LVar's lattice value
-- (represented as a delta) and the event of the LVar freezing.  The listener is
-- given access to a bag token, allowing it to remove itself from the bag of
-- listeners, after unblocking a threshold read, for example.  It is also given
-- access to the scheduler queue for the CPU that generated the event, which it
-- can use to add threads.
data Listener d = Listener {
  onUpdate :: d -> B.Token (Listener d) -> SchedState -> IO (),
  onFreeze ::      B.Token (Listener d) -> SchedState -> IO ()
}

data HandlerPool = HandlerPool {
  numHandlers      :: C.Counter,   -- How many handler callbacks are currently
                                   -- running?
  blockedOnQuiesce :: B.Bag ClosedPar
}

-- | A monadic type constructor for deterministic parallel computations
-- producing an answer @a@.
newtype Par a = Par {
  -- the computation is represented in CPS
  close :: (a -> ClosedPar) -> ClosedPar  
}

-- A "closed" Par computation is one that has been plugged into a continuation.
-- It is represented in a "Church encoded" style, i.e., directly in terms of its
-- interpretation into the IO monad.  Since the continuation has already been
-- plugged into the computation, there is no answer type here.
newtype ClosedPar = ClosedPar {
  exec :: SchedState -> IO ()
}

type SchedState = Sched.State ClosedPar LVarID

instance Functor Par where
  fmap f m = Par $ \k -> close m (k . f)

instance Monad Par where
  return a = Par $ \k -> k a
  m >>= c  = Par $ \k -> close m $ \a -> close (c a) k

instance Applicative Par where
  (<*>) = ap
  pure  = return
  
------------------------------------------------------------------------------
-- A few auxiliary functions
------------------------------------------------------------------------------  

mkPar :: ((a -> ClosedPar) -> SchedState -> IO ()) -> Par a
mkPar f = Par $ \k -> ClosedPar $ \q -> f k q

whenJust :: Maybe a -> (a -> IO ()) -> IO ()
whenJust Nothing  _ = return ()
whenJust (Just a) f = f a

isFrozen :: LVar a d -> IO Bool
isFrozen (LVar {status}) = do
  curStatus <- readIORef status
  case curStatus of
    Active _ -> return False
    Frozen   -> return True
    
------------------------------------------------------------------------------
-- LVar operations
------------------------------------------------------------------------------
    
-- | Create an LVar
newLV :: IO a -> Par (LVar a d)
newLV init = mkPar $ \k q -> do
  state     <- init
  listeners <- B.new
  status    <- newIORef $ Active listeners
  name      <- newLVID
  exec (k $ LVar {state, status, name}) q

-- | Do a threshold read on an LVar
getLV :: (LVar a d)                  -- ^ the LVar 
      -> (a -> Bool -> IO (Maybe b)) -- ^ already past threshold?
      -> (d ->         IO (Maybe b)) -- ^ does d pass the threshold?
      -> Par b
getLV lv@(LVar {state, status}) globalThresh deltaThresh = mkPar $ \k q ->
  let unblockWhen thresh tok q = do
        tripped <- thresh
        whenJust tripped $ \b -> do
          B.remove tok 
          Sched.pushWork q (k b)
      onUpdate d = unblockWhen $ deltaThresh d
      onFreeze   = unblockWhen $ globalThresh state True
  in do

    -- tradeoff: we fastpath the case where the LVar is already beyond the
    -- threshhold by polling *before* enrolling the callback.  The price is
    -- that, if we are not currently above the threshhold, we will have to poll
    -- *again* after enrolling the callback.  This race may also result in the
    -- continuation being executed twice, which is permitted by idempotence.

    curStatus <- readIORef status
    case curStatus of
      Frozen -> do 
        tripped <- globalThresh state True
        case tripped of
          Just b -> exec (k b) q -- already past the threshold; invoke the
                                 -- continuation immediately                    
          Nothing -> sched q     
      Active listeners -> do
        tripped <- globalThresh state False
        case tripped of
          Just b -> exec (k b) q -- already past the threshold; invoke the
                                 -- continuation immediately        
          
          Nothing -> do          -- *transiently* not past the threshhold; block        
            -- add listener, i.e., move the continuation to the waiting bag
            tok <- B.put listeners $ Listener onUpdate onFreeze

            -- but there's a race: the threshold might be passed (or the LVar
            -- frozen) between our check and the enrollment as a listener, so we
            -- must poll again
            frozen <- isFrozen lv
            tripped' <- globalThresh state frozen
            case tripped' of
              Just b -> do
                B.remove tok  -- remove the listener we just added, and
                exec (k b) q  -- execute the continuation. this work might be
                              -- redundant, but by idempotence that's OK
              Nothing -> sched q


-- | Update an LVar
putLV :: LVar a d            -- ^ the LVar
      -> (a -> IO (Maybe d)) -- ^ how to do the put, and whether the LVar's
                             -- value changed
      -> Par ()
putLV LVar {state, status, name} doPut = mkPar $ \k q -> do  
  Sched.setStatus q name         -- publish our intent to modify the LVar
  delta <- doPut state           -- possibly modify the LVar
  curStatus <- readIORef status  -- read the frozen bit *while q's status is marked*
  Sched.setStatus q noName       -- retract our modification intent
  whenJust delta $ \d -> do
    case curStatus of
      Frozen -> error "Attempt to change a frozen LVar"
      Active listeners -> 
        B.foreach listeners $ \(Listener onUpdate _) tok -> onUpdate d tok q
  exec (k ()) q

-- | Freeze an LVar (limited nondeterminism)
freezeLV :: LVar a d -> Par ()
freezeLV LVar {name, status} = mkPar $ \k q -> do
  oldStatus <- atomicModifyIORef status $ \s -> (Frozen, s)    
  case oldStatus of
    Frozen -> return ()
    Active listeners -> do
      Sched.await q (name /=)  -- wait until all currently-running puts have
                               -- snapshotted the active status
      B.foreach listeners $ \Listener {onFreeze} tok -> onFreeze tok q
  exec (k ()) q
  
------------------------------------------------------------------------------
-- Handler pool operations
------------------------------------------------------------------------------  

-- | Create a handler pool
newPool :: Par HandlerPool
newPool = mkPar $ \k q -> do
  cnt <- C.new
  bag <- B.new
  exec (k $ HandlerPool cnt bag) q

-- | Add a handler to an existing pool
addHandler :: HandlerPool                 -- ^ pool to enroll in 
           -> LVar a d                    -- ^ LVar to listen to
           -> (a -> IO (Maybe (Par ())))  -- ^ initial callback
           -> (d -> IO (Maybe (Par ())))  -- ^ subsequent callbacks: updates
           -> Par ()
addHandler hp LVar {state, status} globalThresh updateThresh = 
  let cnt = numHandlers hp    
      
      spawnWhen thresh q = do
        tripped <- thresh
        whenJust tripped $ \cb -> do
          C.inc cnt   -- record callback invocation in pool        
          
          -- create callback thread, which is responsible for recording its
          -- termination in the handler pool
          Sched.pushWork q $ close cb onFinishCB
          
      -- what to do when a callback thread completes
      onFinishCB _ = ClosedPar $ \q -> do
        C.dec cnt                 -- record callback completion in pool
        quiescent <- C.poll cnt   -- check for (transient) quiescence
        when quiescent $          -- wake any threads waiting on quiescence
          let invoke t tok = do
                B.remove tok
                Sched.pushWork q t                
          in B.foreach (blockedOnQuiesce hp) invoke
        sched q
        
      onUpdate d _ q = spawnWhen (updateThresh d) q
      onFreeze   _ _ = return ()
        
  in mkPar $ \k q -> do
    curStatus <- readIORef status 
    case curStatus of
      Active listeners ->             -- enroll the handler as a listener
        do B.put listeners $ Listener onUpdate onFreeze; return ()
      Frozen -> return ()             -- frozen, so no need to enroll 
    spawnWhen (globalThresh state) q  -- poll globally to see whether we should
                                      -- launch any callbacks now
    exec (k ()) q 

-- | Block until a handler pool is quiescent      
quiesce :: HandlerPool -> Par ()
quiesce (HandlerPool cnt bag) = mkPar $ \k q -> do
  -- tradeoff: we assume that the pool is not yet quiescent, and thus enroll as
  -- a blocked thread prior to checking for quiescence
  tok <- B.put bag (k ())
  quiescent <- C.poll cnt
  if quiescent then do
    B.remove tok
    exec (k ()) q 
  else sched q

-- | Freeze an LVar after a given handler quiesces
freezeLVAfter :: LVar a d                    -- ^ the LVar of interest
              -> (a -> IO (Maybe (Par ())))  -- ^ initial callback
              -> (d -> IO (Maybe (Par ())))  -- ^ subsequent callbacks: updates
              -> Par ()
freezeLVAfter lv globalCB updateCB = do
  hp <- newPool
  addHandler hp lv globalCB updateCB
  quiesce hp
  freezeLV lv

------------------------------------------------------------------------------
-- Par monad operations
------------------------------------------------------------------------------

-- | Fork a child thread
fork :: Par () -> Par ()
fork child = mkPar $ \k q -> do
  Sched.pushWork q (k ()) -- "Work-first" policy.
  exec (close child $ const (ClosedPar sched)) q
  -- Sched.pushWork q (close child emptyCont) -- "Help-first" policy.  Generally bad.
  --   exec (k ()) q

-- | Perform an IO action
liftIO :: IO a -> Par a
liftIO io = mkPar $ \k q -> do
  r <- io
  exec (k r) q

-- | Cooperatively schedule other threads
yield :: Par ()  
yield = mkPar $ \k q -> do
  Sched.yieldWork q (k ())
  sched q
  
{-# INLINE sched #-}
sched :: SchedState -> IO ()
sched q = do
  n <- Sched.next q
  case n of
    Just t  -> exec t q
    Nothing -> return ()

-- Forcing evaluation of a LVar is fruitless.
instance NFData (LVar a d) where
  rnf _ = ()
  
{-# INLINE runPar_internal #-}
runPar_internal :: Par a -> IO a
runPar_internal c = do
  queues <- Sched.new numCapabilities noName
  
  -- We create a thread on each CPU with forkOn.  The CPU on which
  -- the current thread is running will host the main thread; the
  -- other CPUs will host worker threads.
  main_cpu <- Sched.currentCPU
  answerMV <- newEmptyMVar

#if 1
  wrkrtids <- newIORef []
  let forkit = forM_ (zip [0..] queues) $ \(cpu, q) -> do 
        tid <- forkWithExceptions (forkOn cpu) "worker thread" $
                 if cpu == main_cpu 
                   then let k x = ClosedPar $ \q -> do 
                              sched q      -- ensure any remaining, enabled threads run to 
                              putMVar answerMV x  -- completion prior to returning the result
                        in exec (close c k) q
                   -- Note: The above is important: it is sketchy to leave any workers running after
                   -- the main thread exits.  Subsequent exceptions on child threads, even if
                   -- forwarded asynchronously, can arrive much later at the main thread
                   -- (e.g. after it has exited, or set up a new handler, etc).
                   else sched q
        atomicModifyIORef_ wrkrtids (tid:)
  putStrLn "About to fork..."      
  ans <- E.catch (forkit >> takeMVar answerMV)
    (\ (e :: E.SomeException) -> do 
        tids <- readIORef wrkrtids
        putStrLn$ "Killing off workers.. "++show tids
        mapM_ killThread tids
        -- if length tids < length queues then do -- TODO: we could try to chase these down in the idle list.
        error ("EXCEPTION in runPar: "++show e)
    )
  putStrLn "parent thread escaped unscathed"
  return ans
#else
  let runWorker (cpu, q) = do 
        if (cpu /= main_cpu)
           then sched q
           else let k x = ClosedPar $ \q -> do 
                      sched q      -- ensure any remaining, enabled threads run to 
                      putMVar answerMV x  -- completion prior to returning the result
                in exec (close c k) q

  -- Here we want a traditional, fork-join parallel loop with proper exception handling:
  let loop [] asyncs = mapM_ wait asyncs
      loop ((cpu,q):tl) asyncs = 
--         withAsync (runWorker state)
        withAsyncOn cpu (runWorker (cpu,q))
                    (\a -> loop tl (a:asyncs))

----------------------------------------
-- (1) There is a BUG in 'loop' presently:
--    "thread blocked indefinitely in an STM transaction"
--  loop (zip [0..] queues) []
----------------------------------------
-- (2) This has the same problem as 'loop':
--  ls <- mapM (\ pr@(cpu,_) -> Async.asyncOn cpu (runWorker pr)) (zip [0..] queues)
--  mapM_ wait ls
----------------------------------------
-- (3) Using this FOR NOW, but it does NOT pin to the right processors:
  mapConcurrently runWorker (zip [0..] queues)
----------------------------------------
   -- Now that child threads are done, it's safe for the main thread
   -- to call it quits.
  takeMVar answerMV  
#endif

-- RRN: Alas, with quasi-determinism we won't be able to get away with this anymore.
-- It would be nice to put freeze in a separate module and be able to choose between
-- runPar or freeze(+runParIO).  But there's no easy way to get this at the module
-- level without making a newtype for Par.  (SafeHaskell can't be triggered on
-- *combinations* of modules, e.g. "you may import the module with freeze, or the
-- module with runPar, but not both".)
-- 
--   Alas, making a newtype is especially painful because of all the additional data
-- structures that would have to be wrapped/reexported OR generalized by a type class
-- (a la ParFuture, ParIVar).
runPar :: Par a -> a
runPar = unsafePerformIO . runPar_internal

-- | A version that avoids an internal `unsafePerformIO` for calling
-- contexts that are already in the `IO` monad.
runParIO :: Par a -> IO a
runParIO = runPar_internal

{-# INLINE atomicModifyIORef_ #-}
atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ ref fn = atomicModifyIORef ref (\ x -> (fn x,()))
