{-# LANGUAGE TemplateHaskell #-}

module Main (tests, main) where

-- import Control.LVish
-- import qualified Control.Par.Class as PC
-- import Control.Par.Class.Unsafe (ParMonad(internalLiftIO))
-- import Control.LVish.Unsafe
-- import Data.LVar.IVar as IV

import Data.Maybe (fromMaybe)
import Data.Word

-- import Control.Concurrent
-- import Control.Monad.Trans

import Control.Exception (evaluate)
import Control.Par.Class (Generator(..))
import Data.Par.Range
import qualified Control.Monad.Par as P

-- import Data.List
import Test.HUnit (Assertion, assert, assertEqual, assertBool, Counts(..))
import Test.Framework.Providers.HUnit
import Test.Framework -- (Test, defaultMain, testGroup)
import Test.Framework.TH (testGroupGenerator)

import TestHelpers (numElems, timeit)

--------------------------------------------------------------------------------

size :: Int
size = fromMaybe 100 numElems

-- Sum from 1..N
expectedSum :: Word64
expectedSum = (s * (s + 1)) `quot` 2
  where s = fromIntegral size

-- About 0.3s for 500M on a laptop:
case_seqfold :: Assertion
case_seqfold = do
  assertEqual "Fold a range of ints" expectedSum =<<
    (timeit $ evaluate $
     fold (\ x y -> x + fromIntegral y) 0 $ irange 1 size)

-- About 0.44s for 500M ints on the same laptop.  That is -- it's slower.
case_seqfoldM :: Assertion
case_seqfoldM = do
  assertEqual "Fold a range of ints in Par" expectedSum =<<
    (timeit $ foldM (\ x y -> return $! x + fromIntegral y) 0 $ irange 1 size)

-- Same thing in a different monad.  This gets about the same time 0.45s.
case_seqfoldMP :: Assertion
case_seqfoldMP = do
  assertEqual "Fold a range of ints in IO" expectedSum =<<
    (timeit $ P.runParIO $
     foldMP (\ x y -> return $! x + fromIntegral y) 0 $ irange 1 size)

-- --------------------------------------------------------------------------------

tests :: Test
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain [tests]

