-- #!/usr/bin/env runghc -i..

{-# LANGUAGE CPP #-}

-- | This module aggregates all the unit tests in this directory.

module Main where

import Test.Tasty (TestTree, testGroup, defaultMain)
--import TestHelpers (defaultMainSeqTests)

import qualified MemoTests
import qualified LVishAndIVar
import qualified ArrayTests
import qualified LogicalTests
import qualified SkipListTests
--import qualified SNZITests
import qualified PureMapTests
import qualified SLMapTests
import qualified CtrieMapTests

import qualified SatMapTests
-- import qualified LayeredSatMapTests
import qualified SetTests
import qualified MaxPosIntTests
import qualified AddRemoveSetTests
import qualified GenericTests

main :: IO ()
main = defaultMain alltests

--alltests :: [TestTree]
alltests :: TestTree
alltests = testGroup "allTests" 
       [ LVishAndIVar.tests
       , ArrayTests.tests
       , MemoTests.tests
       , LogicalTests.tests
       , MaxPosIntTests.tests
       , SetTests.tests
       , PureMapTests.tests 
--       , LayeredSatMap.tests 
       , SatMapTests.tests
--       , LayeredSatMapTests.tests

       , CtrieMapTests.tests

#ifdef FAILING_TESTS
       -- This was failing, but marking bringing it back online to test again [2014.10.22]:       
       , SLMapTests.tests    -- TODO: close Issue #27, #28 first.  
       , SkipListTests.tests -- Seems to diverge on some sizes on slm2/slm3 [2013.12.07]
--       , SNZITests.tests     -- These have failures still [2013.10.23]

       , GenericTests.tests -- Divergence... debugging [2013.12.07]
#endif
       , AddRemoveSetTests.tests
       ]
