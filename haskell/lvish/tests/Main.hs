#!/usr/bin/env runghc -i..

-- | This module aggregates all the unit tests in this directory.

module Main where

import Test.Framework (defaultMain, Test)

import qualified MemoTests
import qualified LVishAndIVar
--import qualified ArrayTests
import qualified LogicalTests
--import qualified SkipListTests
--import qualified SNZITests
import qualified MapTests
import qualified SetTests
import qualified MaxCounterTests

main :: IO ()
main = defaultMain alltests

alltests :: [Test]
alltests = 
       [ LVishAndIVar.tests
       -- , ArrayTests.tests
       , MemoTests.tests
       , LogicalTests.tests
       -- , SkipListTests.tests
       -- , SNZITests.tests -- These have failures still [2013.10.23]
       , MapTests.tests
       , SetTests.tests
       , MaxCounterTests.tests
       ]

