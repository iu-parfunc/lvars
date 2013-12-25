{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|

Thread-safe Logging.

This module supports logging to memory, serializing messages and deferring the work
of actually printing them.  Another thread can flush the logged messages at its
leisure.

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
import           Data.IORef
import           GHC.Conc hiding (yield)
import           System.IO.Unsafe (unsafePerformIO)
import           System.Environment(getEnvironment)

import Control.LVish.Types
import qualified Control.LVish.SchedIdempotentInternal as Sched

----------------------------------------------------------------------------------------------------

-- | A global log for global log messages.
globalLog :: IORef [String]
globalLog = unsafePerformIO $ newIORef []
{-# NOINLINE globalLog #-}

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
