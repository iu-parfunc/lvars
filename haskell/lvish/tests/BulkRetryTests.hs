{-# LANGUAGE TemplateHaskell, DataKinds, ScopedTypeVariables #-}

-- | Tests for the Data.LVar.PureSet and Data.LVar.SLSet modules.

module BulkRetryTests(tests, runTests) where

import Control.Monad
import Control.Exception
import qualified Data.Set as S
import System.IO (stderr)
import Test.Framework.Providers.HUnit 
import Test.Framework (Test, defaultMain, testGroup)
import Test.HUnit (Assertion, assertEqual, assertBool, Counts(..))
import Test.Framework.TH (testGroupGenerator)
import qualified Test.HUnit as HU
import           TestHelpers as T

import           Control.LVish
import           Control.LVish.DeepFrz (DeepFrz(..), Frzn, Trvrsbl, runParThenFreeze, runParThenFreezeIO)
import           Control.LVish.BulkRetry
import qualified Control.LVish.Logging as L
import qualified Data.LVar.Generic as G
import           Data.LVar.PureSet as IS
import qualified Data.LVar.SLSet as SS
import qualified Data.LVar.IVar as IV
import           Data.LVar.NatArray as NA

--------------------------------------------------------------------------------

tests :: Test
tests = $(testGroupGenerator)

runTests :: IO ()
runTests = defaultMainSeqTests [tests]

--------------------------------------------------------------------------------

case_br_v1 = do
  (logs,Right res) <- runParDetailed (Just(0,4)) [L.OutputTo stderr] False 1 v1
  assertEqual "simple forSpeculative" "hi" res

-- | In the blocking version we should only execute each iteration once, and thus see
-- an exact number of log messages.
v1 = do 
  (na :: NatArray s Int) <- NA.newNatArray 5
  forSpeculative (0,5) $ \ _hub ix -> do
    logDbgLn 1 $ "ForSpeculative, START iter "++show ix
    case ix of
      0 -> void$ NA.get na 1
      1 -> void$ NA.get na 2
      2 -> void$ NA.get na 4
      3 -> void$ NA.get na 4
      4 -> return ()
    NA.put na ix (100 + ix)
    logDbgLn 1 $ "ForSpeculative, END iter "++show ix
    return ()
  return "hi"

case_br_v2 = do
  (logs,res) <- runParDetailed (Just(0,4)) [L.OutputTo stderr] False 1 v2
  case res of
    Left e -> throw e
    Right x -> assertEqual "simple forSpeculative" "hi" x

v2 = do 
  (na :: NatArray s Int) <- NA.newNatArray 5
  forSpeculative (0,5) $ \ hub ix -> do
    logDbgLn 1 $ "ForSpeculative, START iter "++show ix
    case ix of
      0 -> void$ getNB hub na 1
      1 -> void$ getNB hub na 2
      2 -> void$ getNB hub na 4
      3 -> void$ getNB hub na 4
      4 -> return ()
    NA.put na ix (100 + ix)
    logDbgLn 1 $ "ForSpeculative, END iter "++show ix
    return ()
  return "hi"

