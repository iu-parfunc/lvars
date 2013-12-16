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

module Control.LVish.SchedIdempotent
  (
    -- * Basic types and accessors
    LVar(..), state, HandlerPool(),
    Par(..), ClosedPar(..),
    
    -- * Safe, deterministic operations
    yield, newPool, fork, forkHP,
    runPar, runParIO, runParLogged,
    withNewPool, withNewPool_,
    forkWithExceptions,
    
    -- * Quasi-deterministic operations
    quiesce, quiesceAll,

    -- * Debug facilities
    logStrLn, logLnAt_, dbgLvl, printLog, 
       
    -- * Unsafe operations; should be used only by experts to build new abstractions
    newLV, getLV, putLV, putLV_, freezeLV, freezeLVAfter,
    addHandler, liftIO, toss,

    -- * Internal, private bits.
    mkPar, Status(..), sched, Listener(..)
  ) where

import           Control.Monad hiding (sequence, join)
import           Control.Concurrent hiding (yield)
import qualified Control.Exception as E
import           Control.DeepSeq
import           Control.Applicative
import           Control.LVish.MonadToss
import           Debug.Trace(trace)
import           Data.IORef
import           Data.Atomics
import           Data.Typeable
import qualified Data.Concurrent.Counter as C
import qualified Data.Concurrent.Bag as B
import           GHC.Conc hiding (yield)
import           System.IO
import           System.IO.Unsafe (unsafePerformIO)
import           System.Environment(getEnvironment)
import           System.Mem.StableName (makeStableName, hashStableName)
import           Prelude  hiding (mapM, sequence, head, tail)
import           System.Random (random)

#ifdef DEBUG_LVAR               
import           Text.Printf (printf)
#endif

-- import Control.Compose ((:.), unO)
import           Data.Traversable 

import Control.LVish.Types
import qualified Control.LVish.SchedIdempotentInternal as Sched


----------------------------------------------------------------------------------------------------
-- THREAD-SAFE LOGGING
----------------------------------------------------------------------------------------------------

-- This should probably be moved into its own module...

{-# NOINLINE globalLog #-}
globalLog :: IORef [String]
globalLog = unsafePerformIO $ newIORef []

-- | Atomically add a line to the given log.
logStrLn  :: String -> Par ()
logStrLn_ :: String -> IO ()
logLnAt_ :: Int -> String -> IO ()
#ifdef DEBUG_LVAR
#warning "Compiling in LVish DEBUG mode."
logStrLn = liftIO . logStrLn_
logStrLn_ s = logLnAt_ 1 s
logLnAt_ lvl s | dbgLvl >= 5   = putStrLn s
               | dbgLvl >= lvl = atomicModifyIORef globalLog $ \ss -> (s:ss, ())
               | otherwise     = return ()
#else 
logStrLn _  = return ()
logStrLn_ _ = return ()
logLnAt_ _ _ = return ()
{-# INLINE logStrLn #-}
{-# INLINE logStrLn_ #-}
#endif

-- | Print all accumulated log lines.
printLog :: IO ()
printLog = do
  -- Clear the log when we read it:
  lines <- atomicModifyIORef globalLog $ \ss -> ([], ss)
  mapM_ putStrLn $ reverse lines  

-- | The idea here is that while a runPar is underway, we periodically flush out the
-- debug messages.
printLogThread :: IO (IO ())
printLogThread = do
  tid <- forkIO $
         E.catch loop (\ (e :: E.AsyncException) -> do
                        -- One last time on kill:
                        printLog
                        putStrLn " [dbg-log-printer] Shutting down."
                      )
  return (do killThread tid
             let wait = do
                   stat <- threadStatus tid
                   case stat of
                     ThreadRunning -> threadDelay 1000 >> wait
                     _             -> return ()
             wait)
 where
   loop = do
     -- Flush the log at 5Hz:
     printLog
     threadDelay (200 * 1000)
     loop

{-# NOINLINE theEnv #-}
theEnv :: [(String, String)]
theEnv = unsafePerformIO getEnvironment

{-# NOINLINE dbgLvl #-}
-- | Debugging flag shared by several modules.
--   This is activated by setting the environment variable @DEBUG=1..5@.
dbgLvl :: Int
#ifdef DEBUG_LVAR
dbgLvl = case lookup "DEBUG" theEnv of
       Nothing  -> defaultDbg
       Just ""  -> defaultDbg
       Just "0" -> defaultDbg
       Just s   ->
         case reads s of
           ((n,_):_) -> trace (" [!] LVish responding to env Var: DEBUG="++show n) n
           [] -> error$"Attempt to parse DEBUG env var as Int failed: "++show s
#else 
dbgLvl = 0
#endif

defaultDbg :: Int
defaultDbg = 0

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
  = Frozen                       -- ^ further changes to the state are forbidden
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
    Frozen   -> return True
    
------------------------------------------------------------------------------
-- LVar operations
------------------------------------------------------------------------------
    
-- | Create an LVar.
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
                                     -- The @Bool@ indicates whether the LVar is FROZEN.
      -> (d ->         IO (Maybe b)) -- ^ does @d@ pass the threshold?
      -> Par b
getLV lv@(LVar {state, status}) globalThresh deltaThresh = mkPar $ \k q -> do
  -- tradeoff: we fastpath the case where the LVar is already beyond the
  -- threshhold by polling *before* enrolling the callback.  The price is
  -- that, if we are not currently above the threshhold, we will have to poll
  -- /again/ after enrolling the callback.  This race may also result in the
  -- continuation being executed twice, which is permitted by idempotence.

  curStatus <- readIORef status
  case curStatus of
    Frozen -> do 
      tripped <- globalThresh state True
      case tripped of
        Just b -> exec (k b) q -- already past the threshold; invoke the
                               -- continuation immediately                    
        Nothing -> sched q     -- We'll NEVER be above the threshold.
                               -- Shouldn't this be an ERROR? (blocked-indefinitely)
                               -- Depends on our semantics for runPar quiescence / errors states.
    Active listeners -> do
      tripped <- globalThresh state False
      case tripped of
        Just b -> exec (k b) q -- already past the threshold; invoke the
                               -- continuation immediately        

        Nothing -> do          -- /transiently/ not past the threshhold; block        
          
#if GET_ONCE
          execFlag <- newIORef False
#endif
  
          let onUpdate d = unblockWhen $ deltaThresh d
              onFreeze   = unblockWhen $ globalThresh state True
              
              unblockWhen thresh tok q = do
                tripped <- thresh
                whenJust tripped $ \b -> do        
                  B.remove tok
#if GET_ONCE
                  ticket <- readForCAS execFlag
                  unless (peekTicket ticket) $ do
                    (winner, _) <- casIORef execFlag ticket True
                    when winner $ Sched.pushWork q (k b) 
#else 
                  Sched.pushWork q (k b)                     
#endif
          logLnAt_ 4 " [dbg-lvish] getLV: blocking on LVar, registering listeners, returning to sched..."
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


-- | Update an LVar.
putLV_ :: LVar a d                 -- ^ the LVar
       -> (a -> Par (Maybe d, b))  -- ^ how to do the put, and whether the LVar's
                                   -- value changed
       -> Par b
putLV_ LVar {state, status, name} doPut = mkPar $ \k q -> do  
  Sched.setStatus q name         -- publish our intent to modify the LVar
  let cont (delta, ret) = ClosedPar $ \q -> do
        curStatus <- readIORef status  -- read the frozen bit *while q's status is marked*
        Sched.setStatus q noName       -- retract our modification intent
        whenJust delta $ \d -> do
          case curStatus of
            Frozen -> E.throw$ PutAfterFreezeExn "Attempt to change a frozen LVar"
            Active listeners -> 
              B.foreach listeners $ \(Listener onUpdate _) tok -> onUpdate d tok q
        exec (k ret) q 
  exec (close (doPut state) cont) q            -- possibly modify the LVar  
  
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

-- | Create a handler pool.
newPool :: Par HandlerPool
newPool = mkPar $ \k q -> do
  cnt <- C.new
  bag <- B.new
  let hp = HandlerPool cnt bag
  hpMsg " [dbg-lvish] Created new pool" hp
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
            hpMsg " [dbg-lvish] -> Quiescent now.. waking conts" hp 
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
          logLnAt_ 5 " [dbg-lvish] addHandler: Delta threshold triggered, pushing work.."
          closed <- closeInPool hp cb
          Sched.pushWork q closed
      onUpdate d _ q = spawnWhen (updateThresh d) q
      onFreeze   _ _ = return ()

      runWhen thresh q = do
        tripped <- thresh
        whenJust tripped $ \cb -> 
          exec (close cb nullCont) q
  in mkPar $ \k q -> do
    curStatus <- readIORef status 
    case curStatus of
      Active listeners ->             -- enroll the handler as a listener
        do B.put listeners $ Listener onUpdate onFreeze; return ()
      Frozen -> return ()             -- frozen, so no need to enroll

    logLnAt_ 4 " [dbg-lvish] addHandler: calling globalCB.."
    -- At registration time, traverse (globally) over the previously inserted items
    -- to launch any required callbacks.
    exec (close (globalCB state) nullCont) q
    exec (k ()) q 

nullCont = (\() -> ClosedPar (\_ -> return ()))

-- | Block until a handler pool is quiescent.
quiesce :: HandlerPool -> Par ()
quiesce hp@(HandlerPool cnt bag) = mkPar $ \k q -> do
  hpMsg " [dbg-lvish] Begin quiescing pool, identity= " hp
  -- tradeoff: we assume that the pool is not yet quiescent, and thus enroll as
  -- a blocked thread prior to checking for quiescence
  tok <- B.put bag (k ())
  quiescent <- C.poll cnt
  if quiescent then do
    B.remove tok
    hpMsg " [dbg-lvish] -> Quiescent already!" hp
    exec (k ()) q 
  else do 
    hpMsg " [dbg-lvish] -> Not quiescent yet, back to sched" hp
    sched q

-- | A global barrier.
quiesceAll :: Par ()
quiesceAll = mkPar $ \k q -> do
  sched q
  logStrLn_ " [dbg-lvish] Return from global barrier."
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
--  hpMsg " [dbg-lvish] incremented and pushed work in forkInPool, now running cont" hp   
  exec closed q  
  
-- | Fork a child thread.
fork :: Par () -> Par ()
fork f = forkHP Nothing f

-- | Perform an @IO@ action.
liftIO :: IO a -> Par a
liftIO io = mkPar $ \k q -> do
  r <- io
  exec (k r) q
  
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

runPar_internal :: Par a -> IO a
runPar_internal c = do
  closeLogger <- if dbgLvl >= 1
                 then printLogThread
                 else return (return ())    
  res <- runPar_internal2 c
  -- printLog
  closeLogger
  hFlush stdout
  return res

runPar_internal2 :: Par a -> IO a
runPar_internal2 c = do
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
                              sched q            -- ensure any remaining, enabled threads run to 
                              putMVar answerMV x -- completion prior to returning the result
                              -- [TODO: ^ perhaps better to use a binary notification tree to signal the workers to stop...]
                        in exec (close c k) q
                   -- Note: The above is important: it is sketchy to leave any workers running after
                   -- the main thread exits.  Subsequent exceptions on child threads, even if
                   -- forwarded asynchronously, can arrive much later at the main thread
                   -- (e.g. after it has exited, or set up a new handler, etc).
                   else sched q
        atomicModifyIORef_ wrkrtids (tid:)
  logStrLn_ " [dbg-lvish] About to fork workers..."      
  ans <- E.catch (forkit >> takeMVar answerMV)
    (\ (e :: E.SomeException) -> do 
        tids <- readIORef wrkrtids
        logStrLn_$ " [dbg-lvish] Killing off workers due to exception: "++show tids
        mapM_ killThread tids
        -- if length tids < length queues then do -- TODO: we could try to chase these down in the idle list.
        mytid <- myThreadId
        when (dbgLvl >= 1) printLog -- Unfortunately this races with the log printing thread.
        E.throw$ LVarSpecificExn ("EXCEPTION in runPar("++show mytid++"): "++show e)
    )
  logStrLn_ " [dbg-lvish] parent thread escaped unscathed"
  return ans
#else
  -- This was an experiment to use Control.Concurrent.Async to deal with exceptions:
  ----------------------------------------------------------------------------------
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


-- | Run a deterministic parallel computation as pure.
runPar :: Par a -> a
runPar = unsafePerformIO . runPar_internal

-- | A version that avoids an internal `unsafePerformIO` for calling
-- contexts that are already in the `IO` monad.
runParIO :: Par a -> IO a
runParIO = runPar_internal

-- | Debugging aid.  Return debugging logs, in realtime order, in addition to the
-- final result.
runParLogged :: Par a -> IO ([String],a)
runParLogged c =
  do res <- runPar_internal2 c
     lines <- atomicModifyIORef globalLog $ \ss -> ([], ss)
     return (reverse lines, res)

{-# INLINE atomicModifyIORef_ #-}
atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ ref fn = atomicModifyIORef ref (\ x -> (fn x,()))

{-# NOINLINE unsafeName #-}
unsafeName :: a -> Int
unsafeName x = unsafePerformIO $ do 
   sn <- makeStableName x
   return (hashStableName sn)

{-# INLINE hpMsg #-}
hpMsg :: String -> HandlerPool -> IO ()
hpMsg msg hp = 
  when (dbgLvl >= 3) $ do
    s <- hpId_ hp
    logLnAt_ 3 $ msg++", pool identity= " ++s

{-# NOINLINE hpId #-}   
hpId :: HandlerPool -> String
hpId hp = unsafePerformIO (hpId_ hp)

hpId_ :: HandlerPool -> IO String
hpId_ (HandlerPool cnt bag) = do
  sn1 <- makeStableName cnt
  sn2 <- makeStableName bag
  c   <- readIORef cnt
  return $ show (hashStableName sn1) ++"/"++ show (hashStableName sn2) ++
           " transient cnt "++show c


-- | Exceptions that walk up the fork tree of threads.
forkWithExceptions :: (IO () -> IO ThreadId) -> String -> IO () -> IO ThreadId
forkWithExceptions forkit descr action = do 
   parent <- myThreadId
   forkit $ do
      tid <- myThreadId
      E.catch action
	 (\ e -> 
           case E.fromException e of 
             Just E.ThreadKilled -> do
-- Killing worker threads is normal now when exception handling, so this chatter is restricted to debug mode:
#ifdef DEBUG_LVAR
               printf "\nThreadKilled exception inside child thread, %s (not propagating!): %s\n" (show tid) (show descr)
#endif
               return ()
	     _  -> do
#ifdef DEBUG_LVAR               
                      printf "\nException inside child thread %s, %s: %s\n" (show descr) (show tid) (show e)
#endif
                      E.throwTo parent (e :: E.SomeException)
	 )

