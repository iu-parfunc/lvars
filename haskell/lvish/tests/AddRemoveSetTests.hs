{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

-- | Tests for the Data.LVar.AddRemoveSet module.

module AddRemoveSetTests(tests, runTests) where

import Control.Concurrent
import Test.Framework.Providers.HUnit 
import Test.Framework (Test, defaultMain, testGroup)
import Test.HUnit (Assertion, assertEqual, assertBool, Counts(..))
import Test.Framework.TH (testGroupGenerator)
import qualified Test.HUnit as HU
import           TestHelpers as T

import qualified Data.Set as S

import qualified Data.LVar.AddRemoveSet as ARS

import           Control.LVish
import           Control.LVish.DeepFrz (DeepFrz(..), Frzn, Trvrsbl, runParThenFreeze, runParThenFreezeIO)
import           Control.LVish.Internal (liftIO)

--------------------------------------------------------------------------------

tests :: Test
tests = $(testGroupGenerator)

runTests :: IO ()
runTests = T.defaultMainSeqTests [tests]

--------------------------------------------------------------------------------

case_v1 :: Assertion
case_v1 = v1 >>= assertEqual "freeze with 3 elements"
          (S.fromList [1..3] :: S.Set Int)

-- If you have a computation that does freezing, you have to run it with runParIO.
v1 :: IO (S.Set Int)
v1 = runParIO $
     do s <- ARS.newEmptySet
        ARS.insert 1 s
        ARS.insert 2 s
        ARS.insert 3 s
        ARS.waitAddedSize 3 s
        ARS.freezeSet s

case_v2 :: Assertion
case_v2 = v2 >>= assertEqual "freeze with 10 elements added, asynchronously"
          (S.fromList [1.. v2size] :: S.Set Int)

v2 :: IO (S.Set Int)
v2 = runParIO $
     do s <- ARS.newEmptySet
        mapM_ (\n -> fork $ do
                     -- liftIO$ threadDelay 5000 
                     logDbgLn 3$ " [AR-v2] Doing one insert: "++show n
                     ARS.insert n s) [1.. v2size]
        logDbgLn 3$ " [AR-v2] now waiting.."
        ARS.waitAddedSize v2size s
        logDbgLn 3$ " [AR-v2] now freezing.."
        ARS.freezeSet s

v2size =
  case numElems of
    Just x -> x
    Nothing -> 10

case_v3 :: Assertion
case_v3 = stressTest T.stressTestReps 15 v3 (\()->True)

-- "freeze with 3 elements added, asynchronously"
-- If we're doing a guaranteed-deterministic computation we can't
-- actually read out the contents of the set.
v3 :: Par d s ()
v3 = 
     do s <- ARS.newEmptySet
        mapM_ (\n -> fork $ ARS.insert n s) [1..10]
        ARS.waitAddedSize 10 s

-- Getting occasional failures here with -N2, don't know what's
-- wrong. :(
case_v4 :: Assertion
case_v4 = stressTest T.stressTestReps 30 v4 (== (S.fromList [1..10] :: S.Set Int))

-- "additions and removals"
v4 :: Par QuasiDet s (S.Set Int)
v4 = 
     do s <- ARS.newEmptySet
        mapM_ (\n -> fork $ ARS.insert n s) [1..15]
        mapM_ (\n -> fork $ ARS.remove n s) [11..15]
        ARS.waitAddedSize 15 s 
        ARS.waitRemovedSize 5 s 
        ARS.freezeSet s

-- This one is intentionally undersynchronized.
case_i1 :: Assertion
case_i1 = do
  allowSomeExceptions ["Attempt to change a frozen LVar"] $
    do x <- i1
       assertEqual "additions and removals, undersynchronized"
         (S.fromList [1..10] :: S.Set Int) x
  return ()

i1 :: IO (S.Set Int)
i1 = runParIO $
     do s <- ARS.newEmptySet
        mapM_ (\n -> fork $ ARS.insert n s) [1..15]
        mapM_ (\n -> fork $ ARS.remove n s) [11..15]
        -- If we don't wait for 15 additions, they might not all be
        -- there when we check.
        ARS.waitRemovedSize 5 s 
        ARS.freezeSet s
