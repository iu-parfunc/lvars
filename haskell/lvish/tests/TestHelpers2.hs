{-# LANGUAGE BangPatterns, CPP, ScopedTypeVariables, RankNTypes #-}

-- | This provides additional helpers that depend specifically on the lvish package.

module TestHelpers2
       (
         stressTest, stressTestReps,
         module TestHelpers
       ) where

import TestHelpers

import Data.Word
import System.IO (hFlush, stdout, stderr, hPutStrLn)
import Control.Concurrent (threadDelay)
import Test.HUnit as HU

-- import Control.LVish.SchedIdempotent (liftIO, dbgLvl, forkWithExceptions)
import Control.LVish (runParDetailed, Par, OutDest(..), DbgCfg(..))
import Debug.Trace

--------------------------------------------------------------------------------

-- | Run a test repeatedly while using the debugging infrastructure to randomly
-- (artificially) vary thread interleavings.  When a schedule resulting in an
-- incorrect answer (or exception) is found, it is printed.
stressTest :: Show a =>
              Word -- ^ Number of repetitions 
           -> Int  -- ^ Number of workers to run on.  MUST be greater than the maximum
                   -- number of tasks that can run in parallel; otherwise this will deadlock.
           -> (forall s . Par d s a)   -- ^ Computation to run
           -> (a -> Bool) -- ^ Test oracle
           -> IO ()
stressTest reps workers comp oracle = 
 do -- rawRun
    reploop reps
 where 
  rawRun = do x <- runParDetailed (DbgCfg (Just(0,0)) [] True) workers comp
              putStr "!"
              checkRes x
              
  reploop 0 = return ()
  reploop i = do 
    x <- runParDetailed (DbgCfg (Just(4,10)) [OutputInMemory, OutputEvents] True) workers comp
    putStr "."
    checkRes x
    reploop (i-1)

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
      writeFile "failing_sched.log" (unlines logs)
      hPutStrLn stderr "Wrote to file: failing_sched.log"
      HU.assertFailure s


defaultNST :: Word
defaultNST = 100

stressTestReps :: Word
{-# NOINLINE stressTestReps #-}
stressTestReps = case lookup "STRESSTESTS" theEnv of
       Nothing  -> defaultNST
       Just ""  -> defaultNST
       Just s   ->
         case reads s of
           ((n,_):_) -> trace (" [!] responding to env Var: STRESSTESTS="++show n) n
           [] -> error$"Attempt to parse STRESSTESTS env var as Int failed: "++show s


