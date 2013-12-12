{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

-- | Tests for the Data.LVar.AddRemoveSet module.

module AddRemoveSetTests(tests, runTests) where

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

--------------------------------------------------------------------------------

tests :: Test
tests = $(testGroupGenerator)

runTests :: IO ()
runTests = defaultMain [tests]

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
