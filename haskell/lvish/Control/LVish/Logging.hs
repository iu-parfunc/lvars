{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

{-|

Thread-safe Logging with bonus controlled-schedule debugging capabilities.

This module supports logging to memory, serializing messages and deferring the work
of actually printing them.  Another thread can flush the logged messages at its
leisure.

The second capability of this infrastructure is to use the debugging print messages
as points at which to gate the execution of the program.  That is, each `logStrLn_`
call becomes a place where the program blocks and checks in with a central
coordinator, which only allows one thread to unblock at a time.  Thus, if there are
sufficient debug logging messages in the program, this can enable a form of
deterministic replay (and quickcheck-style testing of different interleavings).

 -}

module Control.LVish.Logging
       (
         -- * Log to a shared, global log
         printLog, printLogThread,
         logStrLn_, logLnAt_,

         -- * Global variables
         globalLog, dbgLvl
       )
       where

import qualified Control.Exception as E
import qualified Control.Concurrent.Async as A
import           Data.IORef
import           GHC.Conc hiding (yield)
import           Control.Concurrent
import           System.IO.Unsafe (unsafePerformIO)
import           System.Environment(getEnvironment)

import Control.LVish.Types
import qualified Control.LVish.SchedIdempotentInternal as Sched

----------------------------------------------------------------------------------------------------

-- | A Logger coordinates a set of threads that print debug logging messages.
--
--   This are abstract objects supporting only the operations provided by this module
--   and the non-hidden fields of the Logger.
data Logger = Logger { coordinator :: A.Async () -- ThreadId
                                      -- ^ (private) The thread that chooses which action to unblock next
                                      -- and handles printing to the screen as well.
                     , checkPoint :: Chan Writer -- ^ The serialized queue of writers attempting to log dbg messages.
                     , logged :: IORef [LogMsg] -- ^ (private) The actual log of messages.
                     , closeIt :: IO () -- ^ (public) A method to complete flushing, close down the helper thread,
                                        -- and generally wrap up.
                     , numWorkers :: Int -- ^ How many threads must check-in each round before proceeding?
                     }

-- | A single thread attempting to log a message.  It only unblocks when the attached
-- MVar is filled.
data Writer = Writer { who :: String
                     , continue :: MVar ()
                     , msg :: LogMsg
                       -- TODO: Indicate whether this writer has useful work to do or
                       -- is about to block... this provides a simple notion of
                       -- priority.
                     }

-- | We allow logging in O(1) time in String or ByteString format.  In practice the
-- distinction is not that important, because only *thunks* should be logged; the
-- thread printing the logs should deal with forcing those thunks.
data LogMsg = StrMsg String 

-- | Create a new logger, which includes forking a coordinator thread.
--   Takes as argument the number of worker threads participating in the computation.
newLogger :: Int -> IO Logger
newLogger numWorkers = do
  logged <- newIORef []
  checkPoint <- newChan
  coordinator <- A.async $ do
    -- Proceed in rounds, gather the set of actions that may happen in parallel, then
    -- pick one.  We log the series of decisions we make for reproducability.
    let loop = do
          -- PROBLEM: how do we detect quiescence?  How do we know how many threads
          -- should check-in?
          --
          -- HEURISTIC: require that we be initialized with a number of worker
          -- threads.  ALL workers are expected to check-in in some form each round.
          error "FINISHME"
          yield
          loop
    return ()
  let closeIt = A.cancel coordinator
  return $! Logger { coordinator, logged, checkPoint, closeIt, numWorkers  }

----------------------------------------------------------------------------------------------------

-- | A target for global log messages.
globalLogger :: Logger
globalLogger = unsafePerformIO $ newLogger numCapabilities
{-# NOINLINE globalLogger #-}


-- | A global log for global log messages.
globalLog :: IORef [String]
globalLog = unsafePerformIO $ newIORef []
{-# NOINLINE globalLog #-}

----------------------------------------------------------------------------------------------------

-- | The global coordinator that all threads check in with before proceeding.



  
-- | Atomically add a line to the given log.
logStrLn_ :: String -> IO ()
logLnAt_ :: Int -> String -> IO ()
#ifdef DEBUG_LVAR
#warning "Compiling in LVish DEBUG mode."
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

-- | Debugging flag shared by several modules.
--   This is activated by setting the environment variable @DEBUG=1..5@.
-- 
--   By convention @DEBUG=100@ turns on full sequentialization of the program and
--   control over the interleavings in concurrent code, enabling systematic debugging
--   of concurrency problems.
dbgLvl :: Int
#ifdef DEBUG_LVAR
{-# NOINLINE dbgLvl #-}
dbgLvl = case lookup "DEBUG" theEnv of
       Nothing  -> defaultDbg
       Just ""  -> defaultDbg
       Just "0" -> defaultDbg
       Just s   ->
         case reads s of
           ((n,_):_) -> trace (" [!] LVish responding to env Var: DEBUG="++show n) n
           [] -> error$"Attempt to parse DEBUG env var as Int failed: "++show s
#else 
{-# INLINE dbgLvl #-}
dbgLvl = 0
#endif

defaultDbg :: Int
defaultDbg = 0

replayDbg :: Int
replayDbg = 100
