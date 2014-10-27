{-# LANGUAGE Unsafe #-}
{-# LANGUAGE RankNTypes #-} 
{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns, BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-} 
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-} -- For DeepFreeze

{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

-- | This is an internal module that provides the core parallel scheduler.
--   It is /not/ for end-users.

module Internal.Control.LVish.SchedIdempotent
  (
    -- * Basic types and accessors
    LVar(..), state, HandlerPool(),
    Par(..), ClosedPar(..),
    
    -- * Safe, deterministic operations
    yield, newPool, fork, forkHP,
    runPar, runParIO,
    runParDetailed, runParLogged,
    withNewPool, withNewPool_,
    forkWithExceptions,
    
    -- * Quasi-deterministic operations
    quiesce, quiesceAll,

    -- * Re-exported debug facilities
    logStrLn, dbgLvl, getLogger,
       
    -- * Unsafe operations; should be used only by experts to build new abstractions
    newLV, getLV, putLV, putLV_, freezeLV, freezeLVAfter,
    addHandler, liftIO, toss,

    -- * Internal, private bits.
    mkPar, Status(..), sched, Listener(..), lvarDbgName
  ) where

import           Control.Monad hiding (sequence, join)
import           Control.Concurrent hiding (yield)
import qualified Control.Concurrent as Conc
import qualified Control.Exception as E
import qualified Control.Concurrent.Async as A
import           Control.DeepSeq
import           Control.Applicative
import           Control.LVish.MonadToss
import           Control.LVish.Logging as L
import           Debug.Trace(trace)
import           Data.IORef
import           Data.Atomics
import           Data.Typeable
import qualified Data.Atomics.Counter as C2
import qualified Data.Concurrent.Counter as C
import qualified Data.Concurrent.Bag as B
import           GHC.Conc hiding (yield)
import qualified GHC.Conc 
import           System.IO
import           System.IO.Unsafe (unsafePerformIO)
import           System.Environment(getEnvironment)
import           System.Mem.StableName (makeStableName, hashStableName)
import           Prelude  hiding (mapM, sequence, head, tail)
import qualified Prelude
import           System.Random (random)
import           Text.Printf (printf, hPrintf)

-- import Control.Compose ((:.), unO)
import           Data.Traversable  hiding (forM)

import Control.LVish.Types
import qualified Control.LVish.SchedIdempotentInternal as Sched

------------------------------------------------------------------------------
-- LVar and Par monad representation
------------------------------------------------------------------------------

-- | LVars are parameterized by two types:
-- 
--     * The first, @a@, characterizes the \"state\" of the LVar (i.e., the lattice
--     element), and should be a concurrently mutable data type.  That means, in
--     particular, that only a /transient snapshot/ of the state can be
--     obtained in general.  But the information in such a snapshot is always a
--     lower bound on the current value of the LVar.
-- 
--     * The second, @d@, characterizes the \"delta\" associated with a @putLV@
--     operation (i.e., the actual change, if any, to the LVar's state).
--     In many cases such deltas allow far more efficient communication between
--     @putLV@s and blocked @getLV@s or handlers.  It is crucial, however, that
--     the behavior of a @get@ or handler does not depend on the /particular/
--     choice of @putLV@ operations (and hence deltas) that moved the LVar over
--     the threshold.  For simple data structures, the delta may just be the
--     entire LVar state, but for, e.g., collection data structures, delta will
--     generally represent a single insertion.
data LVar a d = LVar {
  state  :: a,                -- the current, "global" state of the LVar
  status :: {-# UNPACK #-} !(IORef (Status d)), -- is the LVar active or frozen?  
  name   :: {-# UNPACK #-} !LVarID            -- a unique identifier for this LVar
}

type LVarID = IORef ()
newLVID = newIORef ()

-- | a global ID that is *not* the name of any LVar.  Makes it possible to
-- represent Maybe (LVarID) with the type LVarID -- i.e., without any allocation.
noName :: LVarID
noName = unsafePerformIO $ newLVID
{-# NOINLINE noName #-}

-- | The frozen bit of an LVar is tied together with the bag of waiting listeners,
-- which allows the entire bag to become garbage immediately after freezing.
-- (Note, however, that outstanding @put@s that occurred just before freezing
-- may still reference the bag, which is necessary to ensure that all listeners
-- are informed of the @put@ prior to freezing.)
data Status d 
  = Freezing                     -- ^ further changes to the state are forbidden
  | Frozen                       -- ^ further changes to the state are forbidden
  | Active (B.Bag (Listener d))  -- ^ bag of blocked threshold reads and handlers

-- | A listener for an LVar is informed of each change to the LVar's state
-- (represented as a delta) and the event of the LVar freezing.  The listener is
-- given access to a bag token, allowing it to remove itself from the bag of
-- listeners, after unblocking a threshold read, for example.  It is also given
-- access to the scheduler queue for the CPU that generated the event, which it
-- can use to add threads.
data Listener d = Listener {
  onUpdate :: d -> B.Token (Listener d) -> SchedState -> IO (),
  onFreeze ::      B.Token (Listener d) -> SchedState -> IO ()
}

-- | A @HandlerPool@ contains a way to count outstanding parallel computations that
-- are affiliated with the pool.  It detects the condition where all such threads
-- have completed.
data HandlerPool = HandlerPool {
  numHandlers      :: C.Counter,   -- How many handler callbacks are currently
                                   -- running?
  blockedOnQuiesce :: B.Bag ClosedPar
}

-- | A monadic type constructor for parallel computations producing an answer @a@.
-- This is the internal, unsafe type.
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
    _        -> return True

-- | Logging within the (internal) Par monad.
logStrLn  :: Int -> String -> Par ()
#ifdef DEBUG_LVAR
-- logStrLn = liftIO . logStrLn_
logStrLn lvl str = do
  lgr <- getLogger
  when (lvl >= L.minLvl lgr && lvl <= L.maxLvl lgr) $ do
    num <- getWorkerNum
    if lvl < 0
     then liftIO$ logHelper (Just lgr) num (L.OffTheRecord (-lvl) str)
     else liftIO$ logHelper (Just lgr) num (L.StrMsg lvl str)
#else
logStrLn _ _  = return ()
#endif

logHelper :: Maybe Logger -> Int -> LogMsg -> IO ()
logHelper lgr num msg = do
  let msg' = L.mapMsg (("wrkr"++show num++" ")++) msg
  case lgr of 
    Just lgr -> L.logOn lgr msg' -- logOn will do the lvl check
    Nothing  -> hPutStrLn stderr ("WARNING/nologger:"++show msg')

logWith      :: Sched.State a s -> Int -> String -> IO ()
logOffRecord :: Sched.State a s -> Int -> String -> IO ()
#ifdef DEBUG_LVAR
-- Only when the debug level is 1 or higher is the logger even initialized:
logWith      q lvl str = logHelper (Sched.logger q) (Sched.no q) (L.StrMsg lvl str)
logOffRecord q lvl str = logHelper (Sched.logger q) (Sched.no q) (L.OffTheRecord lvl str)
#else
logWith _ _ _ = return ()
logOffRecord  _ _ _  = return ()
#endif

------------------------------------------------------------------------------
-- LVar operations
------------------------------------------------------------------------------

-- | Debugging only -- create some kind of printable identifier for
-- the LVar (uses StableName).
lvarDbgName :: LVar a d -> String
lvarDbgName (LVar {state, status}) = (show$ unsafeName state)
    
-- | Create an LVar.
newLV :: IO a -> Par (LVar a d)
newLV init = mkPar $ \k q -> do  
  logOffRecord q 7$ " [dbg-lvish] newLV: allocating... "
  state     <- init
  listeners <- B.new
  status    <- newIORef $ Active listeners
  name      <- newLVID
  exec (k $ LVar {state, status, name}) q

-- | Do a threshold read on an LVar
getLV :: (LVar a d)                  -- ^ the LVar 
      -> (a -> Bool -> IO (Maybe b)) -- ^ already past threshold?
                                     -- The @Bool@ indicates whether the LVar is FROZEN.
      -> (d ->         IO (Maybe b)) -- ^ does @d@ pass the threshold?
      -> Par b
getLV lv@(LVar {state, status}) globalThresh deltaThresh = mkPar $ \k q -> do
  -- tradeoff: we fastpath the case where the LVar is already beyond the
  -- threshhold by polling *before* enrolling the callback.  The price is
  -- that, if we are not currently above the threshhold, we will have to poll
  -- /again/ after enrolling the callback.  This race may also result in the
  -- continuation being executed twice, which is permitted by idempotence.
  let uniqsuf = ", lv "++lvarDbgName lv++" on worker "++(show$ Sched.no q)
  
  logWith q 7$ " [dbg-lvish] getLV: first readIORef "++uniqsuf
  curStatus <- readIORef status
  case curStatus of
    Active listeners -> do
      logWith q 7$ " [dbg-lvish] getLV (active): check globalThresh"++uniqsuf
      tripped <- globalThresh state False
      case tripped of
        Just b -> exec (k b) q -- already past the threshold; invoke the
                               -- continuation immediately        

        Nothing -> do          -- /transiently/ not past the threshhold; block        

          execFlag <- newDedupCheck
          let onUpdate d = unblockWhen $ deltaThresh d
              onFreeze   = unblockWhen $ globalThresh state True
              {-# INLINE unblockWhen #-}
              unblockWhen thresh tok q = do
                let uniqsuf = ", lv "++(lvarDbgName lv)++" on worker "++(show$ Sched.no q)
                logWith q 7$ " [dbg-lvish] getLV (active): callback: check thresh"++uniqsuf
                tripped <- thresh
                whenJust tripped $ \b -> do        
                  B.remove tok
                  winnerCheck execFlag q (Sched.pushWork q (k b)) (return ())

          logWith q 8$ " [dbg-lvish] getLV "++show(unsafeName execFlag)++
                       ": blocking on LVar, registering listeners..."
          -- add listener, i.e., move the continuation to the waiting bag
          tok <- B.put listeners $ Listener onUpdate onFreeze

          -- but there's a race: the threshold might be passed (or the LVar
          -- frozen) between our check and the enrollment as a listener, so we
          -- must poll again
          logWith q 8$ " [dbg-lvish] getLV (active): second frozen check"++uniqsuf
          frozen <- isFrozen lv
          logWith q 7$ " [dbg-lvish] getLV (active): second globalThresh check"++uniqsuf
          tripped' <- globalThresh state frozen
          case tripped' of
            Just b -> do
              logWith q 7$ " [dbg-lvish] getLV (active): second globalThresh tripped, remove tok"++uniqsuf
              B.remove tok  -- remove the listener we just added, and

              winnerCheck execFlag q (exec (k b) q) (sched q)
                      -- execute the continuation. this work might be
                      -- redundant, but in idempotence-mode that's OK
            Nothing -> sched q

    --------------------------------------------------------------------------------
    -- Freezing or Frozen:
    _ -> do
      logWith q 7$ " [dbg-lvish] getLV (frozen): about to check globalThresh"++uniqsuf
      tripped <- globalThresh state True
      case tripped of
        Just b -> do -- logWith q 9$ " [dbg-lvish] getLV (frozen): thresh met, invoking continuation "++uniqsuf
                     exec (k b) q -- already past the threshold; invoke the
                                  -- continuation immediately                    
        Nothing -> sched q     -- We'll NEVER be above the threshold.
                               -- Shouldn't this be an ERROR? (blocked-indefinitely)
                               -- Depends on our semantics for runPar quiescence / errors states.


{-# INLINE newDedupCheck #-}
{-# INLINE winnerCheck #-}
winnerCheck :: DedupCell -> Sched.State a s  -> IO () -> IO () -> IO ()
newDedupCheck :: IO DedupCell

#if GET_ONCE

#  if 0
type DedupCell = IORef Bool
newDedupCheck = newIORef False -- True means someone has already won.
winnerCheck execFlag q tru fal = do                
  ticket <- readForCAS execFlag
  if (peekTicket ticket) 
    then do logWith q 8 $ " [dbg-lvish] getLV winnerCheck failed.."
            fal
    else do
      (winner, _) <- casIORef execFlag ticket True
      logWith q 8 $ " [dbg-lvish] getLV "++show(unsafeName execFlag)
                 ++" on worker "++ (show$ Sched.no q) ++": winner check? " ++show winner
                 ++ ", ticks " ++ show (ticket, peekTicket ticket)
      if winner then tru else fal
#  else

type DedupCell = C2.AtomicCounter
newDedupCheck = C2.newCounter 0 
winnerCheck execFlag q tru fal = do
  cnt <- C2.incrCounter 1 execFlag
  logWith q 8 $ " [dbg-lvish] getLV "++show(unsafeName execFlag)
             ++" on worker "++ (show$ Sched.no q) ++": winner check? " ++show (cnt==1)
             ++ ", counter val " ++ show cnt
  if cnt==1 then tru else fal

#  endif
#else
type DedupCell = ()
newDedupCheck = return ()
winnerCheck _ _ tr _ = tr
#endif







-- | Update an LVar.
putLV_ :: LVar a d                 -- ^ the LVar
       -> (a -> Par (Maybe d, b))  -- ^ how to do the put, and whether the LVar's
                                   -- value changed
       -> Par b
putLV_ lv@(LVar {state, status, name}) doPut = mkPar $ \k q -> do
  let uniqsuf = ", lv "++(lvarDbgName lv)++" on worker "++(show$ Sched.no q)
      putAfterFrzExn = E.throw$ PutAfterFreezeExn "Attempt to change a frozen LVar"
  logWith q 8 $ " [dbg-lvish] putLV: initial lvar status read"++uniqsuf
  fstStatus <- readIORef status
  case fstStatus of
    Freezing -> putAfterFrzExn
    Frozen   -> putAfterFrzExn
    Active listeners -> do
      logWith q 8 $ " [dbg-lvish] putLV: setStatus,"++uniqsuf
      Sched.setStatus q name         -- publish our intent to modify the LVar
      let cont (delta, ret) = ClosedPar $ \q -> do
            logWith q 8 $ " [dbg-lvish] putLV: read final status before unsetting"++uniqsuf
            sndStatus <- readIORef status  -- read the frozen bit *while q's status is marked*
            logWith q 8 $ " [dbg-lvish] putLV: UN-setStatus"++uniqsuf
            Sched.setStatus q noName       -- retract our modification intent
            -- AFTER the retraction, freezeLV is allowed to set the state to Frozen.
            whenJust delta $ \d -> do
              case sndStatus of
                Frozen -> putAfterFrzExn
                _ -> do
                  logWith q 9 $ " [dbg-lvish] putLV: calling each listener's onUpdate"++uniqsuf
                  B.foreach listeners $ \(Listener onUpdate _) tok -> onUpdate d tok q
            exec (k ret) q
      logWith q 5 $ " [dbg-lvish] putLV: about to mutate lvar"++uniqsuf
      exec (close (doPut state) cont) q


-- | Update an LVar without generating a result.  
putLV :: LVar a d             -- ^ the LVar
      -> (a -> IO (Maybe d))  -- ^ how to do the put, and whether the LVar's
                               -- value changed
      -> Par ()
putLV lv doPut = putLV_ lv doPut'
  where doPut' a = do r <- liftIO (doPut a); return (r, ())

-- | Freeze an LVar (introducing quasi-determinism).
--   It is the data structure implementor's responsibility to expose this as quasi-deterministc.
freezeLV :: LVar a d -> Par ()
freezeLV lv@(LVar {name, status}) = mkPar $ \k q -> do
  let uniqsuf = ", lv "++(lvarDbgName lv)++" on worker "++(show$ Sched.no q)
  logWith q 5 $ " [dbg-lvish] freezeLV: atomic modify status to Freezing"++uniqsuf
  oldStatus <- atomicModifyIORef status $ \s -> (Freezing, s)    
  case oldStatus of
    Frozen   -> return ()
    Freezing -> return ()
    Active listeners -> do
      logWith q 7 $ " [dbg-lvish] freezeLV: begin busy-wait for putter status"++uniqsuf
      Sched.await q (name /=)  -- wait until all currently-running puts have
                               -- snapshotted the active status
      logWith q 7 $ " [dbg-lvish] freezeLV: calling each listener's onFreeze"++uniqsuf
      B.foreach listeners $ \Listener {onFreeze} tok -> onFreeze tok q
      logWith q 7 $ " [dbg-lvish] freezeLV: finalizing status as Frozen"++uniqsuf
      writeIORef status Frozen
  exec (k ()) q
  
------------------------------------------------------------------------------
-- Handler pool operations
------------------------------------------------------------------------------  

-- | Create a handler pool.
newPool :: Par HandlerPool
newPool = mkPar $ \k q -> do
  cnt <- C.new
  bag <- B.new
  let hp = HandlerPool cnt bag
  hpMsg q " [dbg-lvish] Created new pool" hp
  exec (k hp) q
  
-- | Convenience function.  Execute a @Par@ computation in the context of a fresh handler pool.
withNewPool :: (HandlerPool -> Par a) -> Par (a, HandlerPool)
withNewPool f = do
  hp <- newPool
  a  <- f hp
  return (a, hp)
  
-- | Convenience function.  Execute a @Par@ computation in the context of a fresh
-- handler pool, while ignoring the result of the computation.
withNewPool_ :: (HandlerPool -> Par ()) -> Par HandlerPool
withNewPool_ f = do
  hp <- newPool
  f hp
  return hp

data DecStatus = HasDec | HasNotDec

-- | Close a @Par@ task so that it is properly registered with a handler pool.
closeInPool :: Maybe HandlerPool -> Par () -> IO ClosedPar
closeInPool Nothing c = return $ close c $ const (ClosedPar sched)
closeInPool (Just hp) c = do
  decRef <- newIORef HasNotDec      -- in case the thread is duplicated, ensure
                                    -- that the counter is decremented only once
                                    -- on termination
  let cnt = numHandlers hp
      
      tryDecRef = do                -- attempt to claim the role of decrementer
        ticket <- readForCAS decRef
        case peekTicket ticket of
          HasDec    -> return False
          HasNotDec -> do
            (firstToDec, _) <- casIORef decRef ticket HasDec
            return firstToDec
            
      onFinishHandler _ = ClosedPar $ \q -> do
        shouldDec <- tryDecRef      -- are we the first copy of the thread to
                                    -- terminate?
        when shouldDec $ do
          C.dec cnt                 -- record handler completion in pool
          quiescent <- C.poll cnt   -- check for (transient) quiescence
          when quiescent $ do       -- wake any threads waiting on quiescence
            hpMsg q " [dbg-lvish] -> Quiescent now.. waking conts" hp 
            let invoke t tok = do
                  B.remove tok
                  Sched.pushWork q t                
            B.foreach (blockedOnQuiesce hp) invoke
        sched q
  C.inc $ numHandlers hp            -- record handler invocation in pool
  return $ close c onFinishHandler  -- close the task with a special "done"
                                    -- continuation that clears it from the
                                    -- handler pool

-- | Add a handler to an existing pool.
{-# INLINE addHandler #-}
addHandler :: Maybe HandlerPool           -- ^ pool to enroll in, if any
           -> LVar a d                    -- ^ LVar to listen to
           -> (a -> Par ())               -- ^ initial snapshot callback on handler registration
           -> (d -> IO (Maybe (Par ())))  -- ^ subsequent callbacks: updates
           -> Par ()
addHandler hp LVar {state, status} globalCB updateThresh = 
  let spawnWhen thresh q = do
        tripped <- thresh
        whenJust tripped $ \cb -> do
          logWith q 5 " [dbg-lvish] addHandler: Delta threshold triggered, pushing work.."
          closed <- closeInPool hp cb
          Sched.pushWork q closed
      onUpdate d _ q = spawnWhen (updateThresh d) q
      onFreeze   _ _ = return ()

  in mkPar $ \k q -> do
    curStatus <- readIORef status 
    case curStatus of
      Active listeners ->             -- enroll the handler as a listener
        do B.put listeners $ Listener onUpdate onFreeze; return ()
      Frozen   -> return ()           -- frozen, so no need to enroll
      Freezing -> return ()           -- frozen, so no need to enroll

    logWith q 4 " [dbg-lvish] addHandler: calling globalCB.."
    -- At registration time, traverse (globally) over the previously inserted items
    -- to launch any required callbacks.
    exec (close (globalCB state) k) q

-- | Block until a handler pool is quiescent.
quiesce :: HandlerPool -> Par ()
quiesce hp@(HandlerPool cnt bag) = mkPar $ \k q -> do
  hpMsg q " [dbg-lvish] Begin quiescing pool, identity= " hp
  -- tradeoff: we assume that the pool is not yet quiescent, and thus enroll as
  -- a blocked thread prior to checking for quiescence
  tok <- B.put bag (k ())
  hpMsg q " [dbg-lvish] quiesce: poll count" hp
  quiescent <- C.poll cnt
  if quiescent then do
    hpMsg q " [dbg-lvish] already quiesced, remove token from bag" hp
    B.remove tok
    exec (k ()) q 
  else do 
    logOffRecord q 4 " [dbg-lvish] -> Not quiescent yet, back to sched"
    sched q

-- | A global barrier.
quiesceAll :: Par ()
quiesceAll = mkPar $ \k q -> do
  logWith q 1 " [dbg-lvish] quiesceAll: initiating global barrier."
  sched q
  logWith q 1 " [dbg-lvish] quiesceAll: Past global barrier."
  exec (k ()) q

-- | Freeze an LVar after a given handler quiesces.

-- This is quasi-deterministic.
freezeLVAfter :: LVar a d                    -- ^ the LVar of interest
              -> (a -> Par ())               -- ^ initial snapshot callback on handler registration
              -> (d -> IO (Maybe (Par ())))  -- ^ subsequent callbacks: updates
              -> Par ()
freezeLVAfter lv globalCB updateCB = do
  let globalCB' = globalCB
      updateCB' = updateCB
  hp <- newPool
  addHandler (Just hp) lv globalCB' updateCB'
  quiesce hp
  freezeLV lv
  
  

------------------------------------------------------------------------------
-- Par monad operations
------------------------------------------------------------------------------

-- | Fork a child thread, optionally in the context of a handler pool.
forkHP :: Maybe HandlerPool -> Par () -> Par ()
forkHP mh child = mkPar $ \k q -> do
  closed <- closeInPool mh child
  Sched.pushWork q (k ()) -- "Work-first" policy.
--  hpMsg q " [dbg-lvish] incremented and pushed work in forkInPool, now running cont" hp   
  exec closed q  
  
-- | Fork a child thread.
fork :: Par () -> Par ()
fork f = forkHP Nothing f

-- | Perform an @IO@ action.
liftIO :: IO a -> Par a
liftIO io = mkPar $ \k q -> do
  r <- io
  exec (k r) q

-- | IF compiled with debugging support, this will return the Logger used by the
-- current Par session, otherwise it will simply throw an exception.
getLogger :: Par L.Logger
getLogger = mkPar $ \k q -> 
  let Just lgr = Sched.logger q in
  exec (k lgr) q

-- | Return the worker that we happen to be running on.  (NONDETERMINISTIC.)
getWorkerNum :: Par Int
getWorkerNum = mkPar $ \k q -> exec (k (Sched.no q)) q

-- | Generate a random boolean in a core-local way.  Fully nondeterministic!
instance MonadToss Par where  
  toss = mkPar $ \k q -> do  
    g <- readIORef $ Sched.prng q
    let (b, g' ) = random g
    writeIORef (Sched.prng q) g'
    exec (k b) q

-- | Cooperatively schedule other threads.
yield :: Par ()  
yield = mkPar $ \k q -> do
  Sched.yieldWork q (k ())
  sched q
  
{-# INLINE sched #-}
-- | Contract: This scheduler function only returns when ALL worker threads have
-- completed their work and idled.
sched :: SchedState -> IO ()
sched q = do
  n <- Sched.next q
  case n of
    Just t  -> exec t q
    Nothing -> return ()

-- Forcing evaluation of a LVar is fruitless.
instance NFData (LVar a d) where
  rnf _ = ()


-- | A variant with full control over the relevant knobs.
--   
--   Returns a list of flushed debug messages at the end (if in-memory logging was
--   enabled, otherwise the list is empty).
--
--   This version of runPar catches ALL exceptions that occur within the runPar, and
--   returns them via an Either.  The reason for this is that even if an error
--   occurs, it is still useful to observe the log messages that lead to the failure.
--
--  WARNING: if LVish was installed without `-fdebug`, then the only
--  log messages generated will be those explictly created by the user
--  with `logDbgLn`.  All messages from the runtime system and core
--  LVar data structures will be absent.
-- 
runParDetailed :: DbgCfg  -- ^ Debugging config
               -> Int           -- ^ How many worker threads to use. 
               -> Par a         -- ^ The computation to run.
               -> IO ([String], Either E.SomeException a)
runParDetailed cfg@DbgCfg{dbgRange, dbgDests, dbgScheduling } numWrkrs comp = do
-- #ifndef DEBUG_LVAR
--   when (dbgScheduling /= True) $ -- || dbgRange /= Nothing  
--     error "runParDetailed: asked to control scheduling, but compiled without debugging support."
-- #endif
  (lgr,queues) <- Sched.new cfg numWrkrs noName 
    
  -- We create a thread on each CPU with forkOn.  The CPU on which
  -- the current thread is running will host the main thread; the
  -- other CPUs will host worker threads.
  main_cpu <- Sched.currentCPU
  answerMV <- newEmptyMVar

  let grabLogs = do  
        logOffRecord (Prelude.head queues) 1 " [dbg-lvish] parent thread escaped unscathed.  Optionally closing logger."
        case lgr of 
          Nothing -> return []
          Just lgr -> do L.closeIt lgr
                         L.flushLogs lgr -- If in-memory logging is off, this will be empty.
      mlog s = case lgr of 
                 Nothing -> return ()
                 Just l  -> L.logOn l (L.OffTheRecord 4 s)

  -- Use Control.Concurrent.Async to deal with exceptions:
  ----------------------------------------------------------------------------------
  let runWorker :: (Int,Sched.State ClosedPar LVarID) -> IO ()
      runWorker (cpu, q) = do 
        if (cpu /= main_cpu)
           then do logOffRecord q 3 $  " [dbg-lvish] Auxillary worker #"++show cpu++" starting."
                   sched q
                   logOffRecord q 3 $  " [dbg-lvish] Auxillary worker #"++show cpu++" exitting."
           else let k x = ClosedPar $ \q -> do                       
                      logOffRecord q 3 " [dbg-lvish] Final continuation of main worker: reenter sched to cleanup."
                      sched q      -- ensure any remaining, enabled threads run to 
                                   -- completion prior to returning the result
                      -- FIXME: this continuation gets duplicated.
                      logOffRecord q 3 " [dbg-lvish] Main worker: past global barrier, putting answer."
                      b <- tryPutMVar answerMV x
#ifdef GET_ONCE
                      unless b $ error "Final continuation of Par computation was duplicated, in spite of GET_ONCE!"
#endif
                      return ()
                in do logOffRecord q 3 " [dbg-lvish] Main worker thread starting."
                      exec (close comp k) q

  -- Here we want a traditional, fork-join parallel loop with proper exception handling:
  let loop [] asyncs = do tid <- myThreadId
                          mlog $ " [dbg-lvish] (main tid "++show tid++") Wait on at least one async to complete.."
                          (_,x) <- A.waitAnyCatch asyncs
                          -- We could do a binary tree of waitBoth here, but this should work for now:
                          case x of
                            Left e -> return $! Left e
                            Right () -> waitloop asyncs -- If one finishes, all are trying to.
      loop ((cpu,q):tl) asyncs = 
        A.withAsyncOn cpu (runWorker (cpu,q))
                      (\a -> loop tl (a:asyncs))
      waitloop [] = do 
                       mlog " [dbg-lvish] All asyncs complete, read final answer MVar."
                       fmap Right (dbgTakeMVar [] "retrieve final runPar answer, after workers complete" answerMV)
--                       fmap Right (takeMVar answerMV)
      waitloop (hd:tl) = do mlog (" [dbg-lvish] Waiting for one async.. "++show(1+length tl)++" remaining")
                            me <- A.waitCatch hd
                            case me of 
                              Left e    -> return $! Left e
                              Right ()  -> waitloop tl
  ----------------------------------------
  -- (1) There was a BUG in 'loop' at some point:
  --    "thread blocked indefinitely in an STM transaction"
  ans <- loop (zip [0..] queues) []
  ----------------------------------------
  -- (2) This has the same problem as 'loop':
  --  ls <- mapM (\ pr@(cpu,_) -> Async.asyncOn cpu (runWorker pr)) (zip [0..] queues)
  --  mapM_ wait ls
  ----------------------------------------
  -- (3) Using this FOR NOW, but it does NOT pin to the right processors:
  --  A.mapConcurrently runWorker (zip [0..] queues)
  ----------------------------------------
  logs <- grabLogs
  return $! (logs,ans)



defaultRun :: Par b -> IO b
defaultRun = fmap (fromRight . snd) .
             runParDetailed cfg numCapabilities
  where
   cfg = DbgCfg { dbgRange = Just (0,dbgLvl)
                , dbgDests = [L.OutputTo stderr, L.OutputEvents]
                , dbgScheduling  = False }

-- | Run a deterministic parallel computation as pure.
runPar :: Par a -> a
runPar = unsafePerformIO . defaultRun

-- | A version that avoids an internal `unsafePerformIO` for calling
-- contexts that are already in the `IO` monad.
runParIO :: Par a -> IO a
runParIO = defaultRun

-- | Debugging aid.  Return debugging logs, in realtime order, in addition to the
-- final result.  This is like `runParDetailed` but uses the default settings.
--
--  WARNING: if LVish was installed without `-fdebug`, then the only
--  log messages generated will be those explictly created by the user
--  with `logDbgLn`.  All messages from the runtime system and core
--  LVar data structures will be absent.
-- 
runParLogged :: Par a -> IO ([String],a)
runParLogged comp = do
-- #ifndef DEBUG_LVAR
--   error "runParLogged: this function is disabled when LVish is compiled without debugging support."
-- #endif
  (logs,ans) <- runParDetailed 
                   DbgCfg { dbgRange = (Just (0,dbgLvl))
                          , dbgDests = [L.OutputEvents, L.OutputInMemory]
                          , dbgScheduling = False }  
                   numCapabilities comp
  return $! (logs,fromRight ans)

-- | Convert from a Maybe back to an exception.
fromRight :: Either E.SomeException a -> a
fromRight (Right x) = x
fromRight (Left e) = E.throw e

{-# INLINE atomicModifyIORef_ #-}
atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ ref fn = atomicModifyIORef' ref (\ x -> (fn x,()))

{-# NOINLINE unsafeName #-}
unsafeName :: a -> Int
unsafeName x = unsafePerformIO $ do 
   sn <- makeStableName x
   return (hashStableName sn)

{-# INLINE hpMsg #-}
hpMsg :: Sched.State a s -> String -> HandlerPool -> IO ()
hpMsg q msg hp = do
#ifdef DEBUG_LVAR
    s <- hpId_ hp
    logWith q 3 $ msg++", pool identity= " ++s
#else
     return ()
#endif

{-# NOINLINE hpId #-}   
hpId :: HandlerPool -> String
hpId hp = unsafePerformIO (hpId_ hp)

-- | Debugging tool for printing which HandlerPool
hpId_ :: HandlerPool -> IO String
hpId_ (HandlerPool cnt bag) = do
  sn1 <- makeStableName cnt
  sn2 <- makeStableName bag
  c   <- readIORef cnt
  return $ show (hashStableName sn1) ++"/"++ show (hashStableName sn2) ++
           " transient cnt "++show c

-- | For debugging purposes.  This can help us figure out (by an ugly
--   process of elimination) which MVar reads are leading to a "Thread
--   blocked indefinitely" exception.
busyTakeMVar :: [ThreadId] -> String -> MVar a -> IO a
busyTakeMVar tids msg mv = 
  do b <- L.newBackoff maxWait
     try b
 where
 maxWait = 10000 -- nanoseconds
 timeOut = (3 * 1000 * 1000) -- three seconds, only for debugging.
 try bkoff | totalWait bkoff >= timeOut = do
--     error "busyTakeMVar (debugging): time-out expired for waiting on MVar"
   -- when dbg $ do
     tid <- myThreadId
     -- After we've failed enough times, start complaining:
     hPrintf stderr "%s not unblocked yet, for: %s\n" (show tid) msg
     stats <- Prelude.mapM threadStatus tids 
     hPrintf stderr $ "Worker statuses: " ++ show (zip tids stats) ++"\n"
     try =<< L.backoff bkoff 
 try bkoff = do
   x <- tryTakeMVar mv
   case x of
     Just y  -> return y
     Nothing -> try =<< L.backoff bkoff 

#ifdef DEBUG_LVAR
dbgTakeMVar = busyTakeMVar
#else
dbgTakeMVar _ _ = takeMVar
#endif

