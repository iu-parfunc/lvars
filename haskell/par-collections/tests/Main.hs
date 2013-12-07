{-# LANGUAGE TemplateHaskell #-}

module Main (tests, main) where

-- import Control.LVish
-- import qualified Control.Par.Class as PC
-- import Control.Par.Class.Unsafe (ParMonad(internalLiftIO))
-- import Control.LVish.Unsafe
-- import Data.LVar.IVar as IV

-- import Control.Concurrent
-- import Control.Monad.Trans

-- import Data.List
import Test.HUnit (Assertion, assert, assertEqual, assertBool, Counts(..))
import Test.Framework.Providers.HUnit
import Test.Framework -- (Test, defaultMain, testGroup)
import Test.Framework.TH (testGroupGenerator)

import TestHelpers (numElems, timeit)

--------------------------------------------------------------------------------

case_seqfold :: Assertion
case_seqfold = do
  assertEqual "" 1 1

-- --------------------------------------------------------------------------------

tests :: Test
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain [tests]

