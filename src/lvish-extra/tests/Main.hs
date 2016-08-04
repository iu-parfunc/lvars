-- #!/usr/bin/env runghc -i..

-- | This module aggregates all the unit tests in this directory.

module Main where

import Test.Tasty (TestTree, testGroup, defaultMain)

import qualified MemoTests
import qualified ArrayTests
import qualified SatMapTests
--import qualified LayeredSatMapTests
import qualified MaxPosIntTests
import qualified AddRemoveSetTests

main :: IO ()
main = defaultMain alltests

--alltests :: [TestTree]
alltests :: TestTree
alltests = testGroup "allTests" 
       [ ArrayTests.tests
       , MemoTests.tests
       , MaxPosIntTests.tests
       , SatMapTests.tests
--       , LayeredSatMapTests.tests
       , AddRemoveSetTests.tests
       ]
