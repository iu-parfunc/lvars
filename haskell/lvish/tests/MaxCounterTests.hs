{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

-- | Tests for the Data.LVar.MaxCounter module.

module MaxCounterTests(tests, runTests) where

import Test.Framework.Providers.HUnit 
import Test.Framework (Test, defaultMain, testGroup)
import Test.HUnit (Assertion, assertEqual, assertBool, Counts(..))
import Test.Framework.TH (testGroupGenerator)
import qualified Test.HUnit as HU
import           TestHelpers as T

import Control.Concurrent (killThread, myThreadId)

import Data.LVar.MaxCounter
import           Control.LVish
import           Control.LVish.DeepFrz (DeepFrz(..), Frzn, Trvrsbl, runParThenFreeze, runParThenFreezeIO)
import qualified Control.LVish.Internal as I

--------------------------------------------------------------------------------

tests :: Test
tests = $(testGroupGenerator)

runTests :: IO ()
runTests = defaultMain [tests]

--------------------------------------------------------------------------------

case_mc1 :: Assertion
case_mc1 = assertEqual "mc1" (Just ()) $ timeOutPure 0.2 $ runPar $ do
  num <- newMaxCounter 0
  fork $ put num 3
  fork $ put num 4
  waitThresh num 4

case_mc2 :: Assertion
case_mc2 = assertEqual "mc2" () $ runPar $ do
  num <- newMaxCounter 0
  fork $ put num 3
  fork $ put num 4
