{-# LANGUAGE TemplateHaskell, CPP, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Tests for SNZI data structure.
module SkipListTests where

import Test.Framework.Providers.HUnit 
import Test.Framework (Test, defaultMain, testGroup)
-- [2013.09.26] Temporarily disabling template haskell due to GHC bug discussed here:
--   https://github.com/rrnewton/haskell-lockfree/issues/10
import Test.Framework.TH (testGroupGenerator)

import Test.HUnit (Assertion, assertEqual, assertBool, Counts(..))
import Control.Monad
import Control.Concurrent
import GHC.Conc

import Data.Time.Clock
import Data.Word
import Data.IORef
import System.Random (random, mkStdGen)
import Control.LVish.SchedIdempotent (liftIO, dbgLvl, forkWithExceptions, printLog)
import Control.LVish  (logDbgLn_)
import qualified Data.Concurrent.LinkedMap as LM
import qualified Data.Concurrent.SkipListMap as SLM

import Debug.Trace

import TestHelpers as T

--------------------------------------------------------------------------------
-- Parameters and helpers
--------------------------------------------------------------------------------

-- A number of insertions to test that is reasonable.
mediumSize :: Int
mediumSize = case numElems of
               Just x -> x
               Nothing -> 10000

expectedSum :: Word64
expectedSum = (s * (s + 1)) `quot` 2
  where s = fromIntegral mediumSize

-- | An additional check to apply to any SLMs we generate.
sliceCheck :: (Ord k, Show k, Show v) => SLM.SLMap k v -> IO ()
sliceCheck slm = do 
  let sl1 = SLM.toSlice slm
  sz1 <- SLM.sliceSize sl1
  Just (sl2,sl3) <- SLM.splitSlice sl1    
  dbg1 <- SLM.debugShow sl2
  dbg2 <- SLM.debugShow sl3
  sz2 <- SLM.sliceSize sl2
  sz3 <- SLM.sliceSize sl3
  logDbgLn_ 5 $ "HALF 1, sz "++ show sz2++":\n"++dbg1
  logDbgLn_ 5 $ "HALF 2, sz "++ show sz3++":\n"++dbg2
  assertEqual "Splitting consvered size: " sz1 (sz2 + sz3)
  return () 

timeit :: IO a -> IO a 
timeit ioact = do 
   start <- getCurrentTime
   res <- ioact
   end   <- getCurrentTime
   putStrLn$ "SELFTIMED: " ++ show (diffUTCTime end start)
   return res

--------------------------------------------------------------------------------
-- Tests for basic linked maps
--------------------------------------------------------------------------------

lm1 :: IO (String)
lm1 = do
  lm <- LM.newLMap
  LM.NotFound tok <- LM.find lm 1
  LM.tryInsert tok "Hello"
  LM.NotFound tok <- LM.find lm 0
  LM.tryInsert tok " World"
  LM.Found s1 <- LM.find lm 1
  LM.Found s0 <- LM.find lm 0
  return $ s1 ++ s0  
case_lm1 :: Assertion  
case_lm1 = lm1 >>= assertEqual "test sequential insertion for LinkedMap" "Hello World"

halveTest :: (Show k, Show v, Ord k, Eq v) => Maybe k -> [(k,v)] -> IO ()
halveTest mend ls = do
  lm  <- LM.fromList ls
  res <- LM.halve' mend lm
  let capit = case mend of
                Nothing -> id
                Just end -> filter ((< end) . fst)
      ls' = capit ls
  printLog                
  case res of
    Nothing -> assertBool "un-halvable things should be size 0 or 1" (length ls' <= 1)
    Just (l,r) -> do
      l' <- LM.toList l
      r' <- LM.toList r
      let r'' = capit r'
      assertBool "halve should not return empty halves (L)" (length l' > 0)
      assertBool "halve should not return empty halves (R)" (length r'' > 0)
      assertEqual "Halving and joining should yield original" ls' (l' ++ r'')

case_halveTest01 :: Assertion
case_halveTest01 = halveTest Nothing (zip [0] ['a'..])
case_halveTest02 = halveTest Nothing (zip [0,1] ['a'..])
case_halveTest03 = halveTest Nothing (zip [0,1,2] ['a'..])
case_halveTest04 = halveTest Nothing (zip [0..3] ['a'..])
case_halveTest05 = halveTest Nothing (zip [0..10] ['a'..])
case_halveTest06 = halveTest Nothing (zip [0..100] ['a'..])

case_halveTest07 = halveTest (Just 1) (zip [0,1] ['a'..])
case_halveTest08 = halveTest (Just 2) (zip [0,1,2] ['a'..])
case_halveTest09 = halveTest (Just 2) (zip [0..3] ['a'..])
case_halveTest10 = halveTest (Just 7) (zip [0..10] ['a'..])
case_halveTest11 = halveTest (Just 91) (zip [0..100] ['a'..])


--------------------------------------------------------------------------------
-- Tests for concurrent SkipLists
--------------------------------------------------------------------------------

slm1 :: IO (String)
slm1 = do
  slm <- SLM.newSLMap 5
  SLM.putIfAbsent slm 0 $ return "Hello "
  SLM.putIfAbsent slm 1 $ return "World"
  Just s0 <- SLM.find slm 0
  Just s1 <- SLM.find slm 1
  dbg <- SLM.debugShow (SLM.toSlice slm)
  logDbgLn_ 1 dbg; printLog
  return $ s0 ++ s1
  
case_slm1 :: Assertion  
case_slm1 = slm1 >>= assertEqual "test sequential insertion for SkipListMap" "Hello World"  

insertionTest :: [(Int, Int)] -> IO (Bool, Word64)
insertionTest chunks = do
  slm <- SLM.newSLMap 10
  timeit $ do 
     mvars <- forM chunks $ \ (start,end) -> do
       mv <- newEmptyMVar
       forkWithExceptions forkIO "slm2 test thread" $ do
         rgen <- newIORef $ mkStdGen 0
         let flip = do
               g <- readIORef rgen
               let (b, g') = random g
               writeIORef rgen $! g'
               return b

         T.for_ (start, end)$ \n -> void (SLM.putIfAbsentToss slm n (return n) flip)
         putMVar mv ()
       return mv  
     forM_ mvars takeMVar
  -- End timing.  Timing just the insertion phase.
  cs <- SLM.counts slm
  logDbgLn_ 1 $ "After insertions, counts: " ++ show cs
  sliceCheck slm    
  matches <- SLM.foldlWithKey id (\b k v -> if k == v then return b else return False) True slm
  summed  <- SLM.foldlWithKey id (\s _ v -> return $! s + fromIntegral v) 0 slm
  printLog
  return (matches, summed)
--  Just n <- SLM.find slm (slm2Count/2)  -- test find function
--  return n

-- Concurrent insertion of the same values:
slm2 :: IO (Bool, Word64)
slm2 = insertionTest (replicate numCapabilities (1,mediumSize))
case_slm2 :: Assertion  
case_slm2 = slm2 >>= assertEqual "test concurrent insertion for SkipListMap (#2)" (True, expectedSum)

-- Same, but in the opposite order:
-- Takes much longer (in parallel)!! Why?
slm3 :: IO (Bool, Word64)
slm3 = insertionTest (replicate numCapabilities (mediumSize,1))
case_slm3 :: Assertion 
case_slm3 = slm3 >>= assertEqual "test concurrent insertion for SkipListMap (#3)" (True, expectedSum)

slm4 :: IO (Bool, Word64)
slm4 = insertionTest (splitRange numCapabilities (1,mediumSize))
case_slm4 :: Assertion 
case_slm4 = slm4 >>= assertEqual "test concurrent insertion for SkipListMap (#4)" (True, expectedSum)




--------------------------------------------------------------------------------

tests :: Test
tests = $(testGroupGenerator)

runTests :: IO ()
runTests = defaultMain [tests]


{-
Development notes:

[2013.11.29] {Adding to tests, encountered nondeterministic failure}
--------------------------------------------------------------------

This was on my laptop.  It looks like we got the wrong sum:

    $ time ./SkipListTests.exe -t slm2 +RTS -N4 -s
    SkipListTests:
    [5,12,27,65,131,258,585,1285,2497,5001,3686]

      slm2: [Failed]
    test concurrent insertion for SkipListMap
    expected: (True,50005000)
     but got: (True,6795141)

             Test Cases  Total
     Passed  0           0
     Failed  1           1
     Total   1           1

Currently I have to repeat hundreds of times to see it again:

    expected: (True,50005000)
     but got: (True,24777280)

Ok, ChaseLev is off.  I don't think it's losing work because of a bug with that.

Notice that it looks like a glitch with the last level of the skiplist, which ends up
far too small.  The other levels look the right size.  The place it gets cut off,
when it goes wrong, varies wildly:

    SkipListTests:
    [9,19,35,71,141,275,578,1269,2502,4986,532]
    test concurrent insertion for SkipListMap
    expected: (True,50005000)
     but got: (True,141778)
 
-}
