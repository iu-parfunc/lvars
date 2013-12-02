
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

-- | Tests for the generic Par-programming interfaces.

module GenericTests (tests, runTests) where

import Control.Monad
import Data.Maybe (fromMaybe)
import qualified Control.Par.Class as PC
import Test.HUnit (Assertion, assertEqual, assertBool, Counts(..))
import Test.Framework.TH (testGroupGenerator)
import Test.Framework    (defaultMain, Test)
import Test.Framework.Providers.HUnit (testCase) -- For macro-expansion.

import TestHelpers as T
import Control.LVish -- LVarSched instances...
import Data.LVar.IVar as IV
import qualified Data.LVar.SLMap as SM
import qualified Control.Par.Class as PC
import Data.Par.Range (zrange)
import Data.Par.Splittable (pforEach)

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

-- ParFold instance
case_pfold_imap :: Assertion 
case_pfold_imap = runParIO $ do
  mp <- SM.newEmptyMap
  let sz = fromMaybe 100 numElems
  -- pforEach (zrange sz) $ \ ix -> do
  forM_ [0..sz-1] $ \ ix -> do       
    SM.insert ix ix mp

  logDbgLn 1 $ "IMap filled up... freezing"
  fmp <- SM.freezeMap mp
  logDbgLn 3 $ "Frozen: "++show fmp
  let mapper x = do
        logDbgLn 2 $ "Mapping in parallel: "++show x
        return x
      folder x y = do 
        logDbgLn 2 $ "Summing in parallel "++show (x,y)
        return $! x+y 
  summed <- PC.pmapFold mapper folder 0 fmp
  logDbgLn 1 $ "Sum of IMap values: " ++ show summed
  return ()

--------------------------------------------------------------------------------

tests :: Test
tests = $(testGroupGenerator)

runTests :: IO ()
runTests = defaultMain [tests]
