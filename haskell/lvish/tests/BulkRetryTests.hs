{-# LANGUAGE TemplateHaskell, DataKinds, ScopedTypeVariables #-}

-- | Tests for the Data.LVar.PureSet and Data.LVar.SLSet modules.

module BulkRetryTests(tests, runTests) where

import Control.Monad
import Control.Exception
import qualified Data.Set as S
import System.IO (stderr)
import Test.Tasty.HUnit 
import Test.Tasty (TestTree, defaultMain, testGroup)
--import Test.HUnit (Assertion, assertEqual, assertBool, Counts(..))
import Test.Tasty.TH (testGroupGenerator)
import qualified Test.HUnit as HU
import           TestHelpers as T
import GHC.Conc (numCapabilities)

import           Control.LVish
import           Control.LVish.DeepFrz (DeepFrz(..), Frzn, Trvrsbl, runParThenFreeze, runParThenFreezeIO)
import           Control.LVish.BulkRetry
import qualified Control.LVish.Logging as L
import qualified Data.LVar.Generic as G
import           Data.LVar.PureSet as IS
import qualified Data.LVar.SLSet as SS
import qualified Data.LVar.IVar as IV
import           Data.LVar.NatArray as NA

import Data.Par.Splittable (pforEach)
import Data.Par.Range (range, fullpar)

--------------------------------------------------------------------------------

tests :: TestTree
tests = $(testGroupGenerator)

runTests :: IO ()
runTests = defaultMain tests

--------------------------------------------------------------------------------

case_br_v1 = do
  (logs,Right res) <- runParDetailed (DbgCfg Nothing [L.OutputTo stderr] False) numCapabilities v1
  assertEqual "simple par for loop" [100,101,102,103,104] res

-- | In the blocking version we should only execute each iteration once, and thus see
-- an exact number of log messages.
v1 = do 
  (na :: NatArray s Int) <- NA.newNatArray 5
--  forSpeculative (0,5) $ \ _hub ix -> do
  pforEach (fullpar$ range 0 5) $ \ ix -> do  
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
  a <- NA.get na 0
  b <- NA.get na 1
  c <- NA.get na 2
  d <- NA.get na 3
  e <- NA.get na 4
  return [a,b,c,d,e]

case_br_v2 = do
  (logs,res) <- runParDetailed (DbgCfg Nothing [L.OutputTo stderr] False) numCapabilities v2
  case res of
    Left e -> throw e
    Right x -> assertEqual "simple forSpeculative" [100,101,102,103,104] x

-- v2 = do 
--   (na :: NatArray s Int) <- NA.newNatArray 5
--   forSpeculative (0,5) $ \ hub ix -> do
--     logDbgLn 1 $ "ForSpeculative, START iter "++show ix
--     case ix of
--       0 -> void$ getNB hub na 1
--       1 -> void$ getNB hub na 2
--       2 -> void$ getNB hub na 4
--       3 -> void$ getNB hub na 4
--       4 -> return ()
--     NA.put na ix (100 + ix)
--     logDbgLn 1 $ "ForSpeculative, END iter "++show ix
--     return ()
--   return "hi"

v2 = do 
  (na :: NatArray s Int) <- NA.newNatArray 5
  forSpeculative (0,5) $ \ hub ix -> do
    logDbgLn 1 $ "ForSpeculative, START iter "++show ix
    let cont _ = do NA.put na ix (100 + ix)
                    logDbgLn 1 $ "ForSpeculative, END iter "++show ix
    case ix of
      0 -> getNB_cps hub na 1 cont
      1 -> getNB_cps hub na 2 cont
      2 -> getNB_cps hub na 4 cont
      3 -> getNB_cps hub na 4 cont
      4 -> cont 4
    return ()
  logDbgLn 1 $ "DONE with for loop, reading final results."
  a <- NA.get na 0
  b <- NA.get na 1
  c <- NA.get na 2
  d <- NA.get na 3
  e <- NA.get na 4
  return [a,b,c,d,e]


