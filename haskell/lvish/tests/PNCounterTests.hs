{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

-- | Tests for the Data.LVar.PNCounter module.

module PNCounterTests(tests, runTests) where

import Test.Framework.Providers.HUnit 
import Test.Framework (Test, defaultMain, testGroup)
import Test.HUnit (Assertion, assertEqual, assertBool, Counts(..))
import Test.Framework.TH (testGroupGenerator)
import qualified Test.HUnit as HU
import           TestHelpers as T

import qualified Data.Set as S

import qualified Data.LVar.PNCounter as PNC

import           Control.LVish
import           Control.LVish.DeepFrz (DeepFrz(..), Frzn, Trvrsbl, runParThenFreeze, runParThenFreezeIO)

--------------------------------------------------------------------------------

tests :: Test
tests = $(testGroupGenerator)

runTests :: IO ()
runTests = defaultMain [tests]

--------------------------------------------------------------------------------

-- TODO: Write tests.