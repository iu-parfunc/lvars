#!/usr/bin/env runghc -i..

-- | This module aggregates all the unit tests in this directory.

module Main where

import Test.Framework (defaultMain, Test)

import qualified MemoTests
import qualified LVishAndIVar

main :: IO ()
main = defaultMain alltests

alltests :: [Test]
alltests = 
       [ LVishAndIVar.tests
       , MemoTests.tests
       ]

