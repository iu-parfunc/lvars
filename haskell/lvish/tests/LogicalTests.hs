{-# LANGUAGE TemplateHaskell #-}

module LogicalTests where

import Control.LVish
import Data.LVar.IVar as IV

import Test.HUnit (Assertion, assert, assertEqual, assertBool, Counts(..))
-- import Test.QuickCheck ()
import Test.Framework.Providers.HUnit
-- import Test.Framework.Providers.QuickCheck2
import Test.Framework -- (Test, defaultMain, testGroup)
import Test.Framework.TH (testGroupGenerator)

--------------------------------------------------------------------------------
-- TESTS:
--------------------------------------------------------------------------------
  
case_and1 :: Assertion
case_and1 = assertEqual "" False $ runPar $ do
              v <- IV.new
              asyncAnd Nothing (return True) (return False) (IV.put v)
              IV.get v

case_and2 :: Assertion
case_and2 = assertEqual "" False $ runPar $ do
              v <- IV.new
              asyncAnd Nothing (return False) (return False) (IV.put v)
              IV.get v

case_and3 :: Assertion
case_and3 = assertEqual "" True $ runPar $ do
              v <- IV.new
              asyncAnd Nothing (return True) (return True) (IV.put v)
              IV.get v                        

case_and4 :: Assertion
case_and4 = assertEqual "" False $ runPar $ do
              v <- IV.new
              asyncAnd Nothing (return False) (return True) (IV.put v)
              IV.get v

case_or1 :: Assertion
case_or1 = assertEqual "" True $ runPar $ do
              v <- IV.new
              asyncOr Nothing (return True) (return False) (IV.put v)
              IV.get v

case_or2 :: Assertion
case_or2 = assertEqual "" False $ runPar $ do
              v <- IV.new
              asyncOr Nothing (return False) (return False) (IV.put v)
              IV.get v

case_or3 :: Assertion
case_or3 = assertEqual "" True $ runPar $ do
              v <- IV.new
              asyncOr Nothing (return True) (return True) (IV.put v)
              IV.get v                        

case_or4 :: Assertion
case_or4 = assertEqual "" True $ runPar $ do
              v <- IV.new
              asyncOr Nothing (return False) (return True) (IV.put v)
              IV.get v



-- TODO: add ones with explicit timing controls (sleep).


--------------------------------------------------------------------------------

tests :: Test
tests = $(testGroupGenerator)

runTests :: IO ()
runTests = defaultMain [tests]
