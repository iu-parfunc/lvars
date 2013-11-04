
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

-- | Tests for the generic Par-programming interfaces.

module GenericTests (tests, runTests) where

import Control.LVish -- LVarSched instances...
import Data.LVar.IVar as IV

import qualified Control.Par.Class as PC

import Test.HUnit (Assertion, assertEqual, assertBool, Counts(..))
import Test.Framework.TH (testGroupGenerator)
import Test.Framework    (defaultMain, Test)
import Test.Framework.Providers.HUnit (testCase) -- For macro-expansion.

--------------------------------------------------------------------------------


case_toQPar :: Assertion  
case_toQPar = t1 >>= assertEqual "" "hi" 

t1 :: IO String
t1 = runParIO par
 where
  par :: Par QuasiDet s String
  par = do
    iv <- IV.new
    PC.toQPar $ IV.put iv "hi"
    IV.get iv

--------------------------------------------------------------------------------

tests :: Test
tests = $(testGroupGenerator)

runTests :: IO ()
runTests = defaultMain [tests]
