{-# LANGUAGE BangPatterns, CPP, ScopedTypeVariables, RankNTypes #-}
{-# LANGUAGE DataKinds #-}

-- | This provides additional helpers that depend specifically on the lvish package.

module TestHelpers2
       (
         stressTest, stressTestReps,
         module TestHelpers
       ) where

import TestHelpers

import Control.Monad(forM_)
import Control.Exception (SomeException)
import Data.Word
import System.IO (hFlush, stdout, stderr, hPutStrLn)
import System.Random (randomRIO)
import Control.Concurrent (threadDelay)
import Test.HUnit as HU

import Internal.Control.LVish.SchedIdempotent (dbgLvl)
import Control.LVish (runParNonDet, runParDetailed, Par, OutDest(..), DbgCfg(..), defaultMemDbgRange)
import Control.Par.EffectSigs 
import Debug.Trace

--------------------------------------------------------------------------------

-- | Run a test repeatedly while using the debugging infrastructure to randomly
-- (artificially) vary thread interleavings.  When a schedule resulting in an
-- incorrect answer (or exception) is found, it is printed.
stressTest :: forall a . Show a =>
              Word -- ^ Number of repetitions 
           -> Int  -- ^ Number of workers to run on.  MUST be greater than the maximum
                   --   number of tasks that can run in parallel; otherwise this will deadlock.
           -> (forall s . Par (Ef P G F B I) s a)   -- ^ Computation to run
           -> (a -> Bool) -- ^ Test oracle
           -> IO ()
stressTest reps workers comp oracle = do
  -- forM_ [1..reps] (\_ -> rawRun)
#ifdef DEBUG_LVAR
    reploop reps
#else
    putStrLn "WARNING: test running normally because we're compiled without -fdebug mode."
    x <- runParNonDet comp
    checkRes ([],Right x)
#endif            
 where 
  rawRun = do x <- runParDetailed (DbgCfg (Just(0,0)) [] False) workers comp
              putStr "!"
              checkRes x

  -- We take the global debug level being raised up as an indiction
  -- that the user wants to see a bunch of chatter on the screen:
  echoScreen = if dbgLvl >= 1
               then [OutputTo stdout]
               else []
                       
  reploop 0 = return ()
  reploop i = do
    -- putStrLn$  "Running computation in debug mode, logging messages in range: "++show defaultMemDbgRange
    (logs,ans) <- runParDetailed (DbgCfg (Just defaultMemDbgRange)
                                  ([OutputInMemory, OutputEvents] ++ echoScreen)
                                  True) workers comp
-- This will cause problems because some of the messages from lvls 1-3 are sent before the workers are UP:
--    (logs,ans) <- runParDetailed (DbgCfg (Just(0,10)) [OutputInMemory, OutputEvents] True) workers comp
    putStr $ (show$length logs) ++"."
--    putStrLn $ "logs:\n"
--    mapM_ print logs
    checkRes (logs,ans)
    reploop (i-1)

  checkRes :: ([String], Either SomeException a) -> IO ()
  checkRes (logs,res) = 
    case res of
      Left exn                 -> failit logs ("Bad test outcome--exception: "++show exn)
      Right x | not (oracle x) -> failit logs ("Bad test result: "++show x)
              | otherwise      -> return ()

  failit logs s = do 
      threadDelay (500 * 1000)
      hPutStrLn stderr $ "\nlstressTest: Found FAILING schedule, length "++show (length logs)
      hPutStrLn stderr "-----------------------------------"
      mapM_ (hPutStrLn stderr) logs
      hPutStrLn stderr "-----------------------------------"
      num <- randomRIO (0,10000::Int)
      let name = "failing_sched"++ show num ++".log"
      writeFile name (unlines logs)
      hPutStrLn stderr $ "Wrote to file: "++name
      HU.assertFailure s


defaultNST :: Word
defaultNST = 100

-- | Default number of reps.  Controlled by environment var `STRESSTESTS`.
-- Use this to crank up the reps on a test while debugging.
stressTestReps :: Word
{-# NOINLINE stressTestReps #-}
stressTestReps = case lookup "STRESSTESTS" theEnv of
       Nothing  -> defaultNST
       Just ""  -> defaultNST
       Just s   ->
         case reads s of
           ((n,_):_) -> trace (" [!] responding to env Var: STRESSTESTS="++show n) n
           [] -> error$"Attempt to parse STRESSTESTS env var as Int failed: "++show s
