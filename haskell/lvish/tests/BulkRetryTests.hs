{-# LANGUAGE TemplateHaskell, DataKinds, ScopedTypeVariables #-}

-- | Tests for the Data.LVar.PureSet and Data.LVar.SLSet modules.

module BulkRetryTests(tests, runTests) where

import Test.Framework.Providers.HUnit 
import Test.Framework (Test, defaultMain, testGroup)
import Test.HUnit (Assertion, assertEqual, assertBool, Counts(..))
import Test.Framework.TH (testGroupGenerator)
import qualified Test.HUnit as HU
import           TestHelpers as T

import qualified Data.Set as S

import qualified Data.LVar.Generic as G
import Data.LVar.PureSet as IS
import qualified Data.LVar.SLSet as SS
import qualified Data.LVar.IVar as IV

import           Control.LVish
import           Control.LVish.DeepFrz (DeepFrz(..), Frzn, Trvrsbl, runParThenFreeze, runParThenFreezeIO)

import Data.LVar.NatArray as NA
import Control.LVish.BulkRetry

--------------------------------------------------------------------------------

tests :: Test
tests = $(testGroupGenerator)

runTests :: IO ()
runTests = defaultMainSeqTests [tests]

--------------------------------------------------------------------------------

case_br_v1 = assertEqual "simple forSpeculative" "hi" =<< runParIO v1

v1 = do 
  (na :: NatArray s Int) <- NA.newNatArray 5
  forSpeculative (0,5) $ \ hub ix -> do
    logDbgLn 1 $ "ForSpeculative, iter "++show ix
    return ()
  return "hi"
