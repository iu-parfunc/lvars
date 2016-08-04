{-# LANGUAGE BangPatterns, OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module PBBS.Timing
       (runAndReport, calibrate, measureFreq, commaint,
        wait_clocks)
       where
-- module Main where

import           Control.Exception (evaluate)
import           Control.Monad (forM_, when)
import           Data.Word
import           Data.IORef
import           Data.List as L
import           Data.List.Split (chunksOf)
import qualified Data.IntSet as IS
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime)
import           Text.Printf (printf)
import           System.Mem (performGC)
import           System.IO.Unsafe (unsafePerformIO)
import           System.Environment (getEnvironment,getArgs)
import           System.CPUTime.Rdtsc (rdtsc)
-- import           Data.Time.Clock (getCurrentTime)
import           System.CPUTime  (getCPUTime)

-- For representing graphs
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

type WorkRet = (Float)
type WorkFn = (Node -> WorkRet)
type Node = Int

#define FIRSTHIT_RDTSC

runAndReport act =
  do (clocks_per_micro,freq) <- calibrate
     startT <- rdtsc
     t0 <- getCurrentTime
     ----------------------------------------
     -- wait_microsecs (1000 * 100)
     act clocks_per_micro
     ----------------------------------------     
     t1 <- getCurrentTime
     putStrLn $ "SELFTIMED " ++ show (diffUTCTime t1 t0) ++ "\n"

     first <- readIORef first_hit
#ifdef FIRSTHIT_RDTSC
     let first' = first - startT
     putStrLn$"Start time in cycles: "++commaint startT      
     putStrLn $ "First hit in raw clock cycles was " ++ commaint first'
     let nanos = ((1000 * 1000 * 1000 * (fromIntegral first')) `quot` (fromIntegral freq :: Integer))
     putStrLn $ " In nanoseconds: "++commaint nanos
     putStrLn $ "FIRSTHIT " ++ show nanos         
#else         
--     putStrLn$"Start time: "++show t0
--     putStrLn $ "First hit time: " ++ show first
     putStrLn $ "FIRSTHIT " ++ show (diffUTCTime first t0)
#endif         


calibrate :: IO (Double, Word64)
calibrate = do 
  freq <- measureFreq
--  clocks_per_kilosin <- measureSin 1000
  let clocks_per_micro :: Rational 
      clocks_per_micro = (fromIntegral freq) / (1000 * 1000)
--      kilosins_per_micro = clocks_per_micro / (fromIntegral clocks_per_kilosin)
      -- numSins :: Rational       
      -- numSins = (fromIntegral wrk) * kilosins_per_micro * 1000
      -- numSins' = round numSins

--      busy_waiter :: WorkFn
--      busy_waiter n = unsafePerformIO $
--        wait_microsecs (wrk * clocks_per_micro) n
--        wait_sins numSins' n
--        return (0,0)

  printf "CPU Frequency: %s, clocks per microsecond %s\n"
           (commaint freq) (commaint (round clocks_per_micro))

--  printf "Time for 1K sins %s, Ksins per micro %s, numSins %s = %s\n"
--         (show clocks_per_kilosin) 
--         (show (fromRational kilosins_per_micro ::Double))
--         (show numSins) (show numSins')

  return (fromRational clocks_per_micro, freq)




------------------------------------------------------------------------------------------

#ifdef FIRSTHIT_RDTSC
first_hit :: IORef Word64
first_hit = unsafePerformIO$ newIORef maxBound
#else
first_hit :: IORef UTCTime
-- UH: this is the wrong initial setting!
first_hit = unsafePerformIO$ newIORef =<< getCurrentTime
#endif

-- | Busy-wait for a certain number of clock cycles.
wait_clocks :: Word64 -> IO Double -- (Float,Double)
wait_clocks clocks = do
  myT <- rdtsc
#ifdef FIRSTHIT_RDTSC
  atomicModifyIORef' first_hit (\ t -> (min t myT,()))
#else
  myTime <- getCurrentTime
  atomicModifyIORef' first_hit (\ t -> (min t myTime,()))
#endif  
  let loop !n = do
        now <- rdtsc
        if now - myT >= clocks then return n else loop (n+1)
  cnt <- loop 0
  return (fromIntegral cnt)


-- Measure clock frequency, spinning rather than sleeping to try to
-- stay on the same core.
measureFreq :: IO Word64 -- What units is this in? -- LK
measureFreq = do
  let millisecond = 1000 * 1000 * 1000 -- picoseconds are annoying
      -- Measure for how long to be sure?      
--      measure = 200 * millisecond
      measure = 1000 * millisecond      
      scale :: Integer
      scale = 1000 * millisecond `quot` measure
  t1 <- rdtsc 
  start <- getCPUTime
  let loop :: Word64 -> Word64 -> IO (Word64,Word64)
      loop !n !last = 
       do t2 <- rdtsc 
	  when (t2 < last) $
	       putStrLn$ "COUNTERS WRAPPED "++ show (last,t2) 
	  cput <- getCPUTime		
	  if (cput - start < measure)
	   then loop (n+1) t2
	   else return (n,t2)
  (n,t2) <- loop 0 t1
  putStrLn$ "  Approx getCPUTime calls per second: "++ commaint (scale * fromIntegral n)
  when (t2 < t1) $ 
    putStrLn$ "WARNING: rdtsc not monotonically increasing, first "++show t1++" then "++show t2++" on the same OS thread"

  return$ fromIntegral (fromIntegral scale * (t2 - t1))


-- Readable large integer printing:
commaint :: (Show a, Integral a) => a -> String
commaint n = 
   reverse $ concat $
   intersperse "," $ 
   chunksOf 3 $ 
   reverse (show n)


