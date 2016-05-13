{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}

-- | Tests for the Data.LVar.PureMap and Data.LVar.SLMap modules.

module LayeredSatMapTests(tests, runTests, fillNFreeze) where

import Data.LVar.PureSet as IS
import qualified Data.LVar.LayeredSatMap as IM 
  -- The common interface under test:
  (LayeredSatMap, insert, newEmptyMap, newFromList, 
   -- Not sure yet if we will get these:      
   -- freezeMap, unionHP, forEach, forEachHP, traverseMap, traverseMapHP
   )

-- TODO: Use backpack for this when it is available:
#include "CommonMapWriteTests.hs"

type TheMap k s v = IM.LayeredSatMap k s v 

--------------------------------------------------------------------------------

tests :: Test
tests = testGroup "" [testsHere, tests_writeOnly ]

testsHere :: Test
testsHere = $(testGroupGenerator)

runTests :: IO ()
runTests = defaultMainSeqTests [tests]

------------------------------------------------------------------------------------------
-- Show instances
------------------------------------------------------------------------------------------


-- case_show03 :: Assertion
-- case_show03 = assertEqual "show for LayeredSatMap" "{LayeredSatMap: (\"key1\",33), (\"key2\",44)}" show03
-- show03 :: String
-- show03 = show$ runParThenFreeze $ do
--   mp <- IM.newEmptyMap
--   IM.insert "key1" (33::Int) mp
--   IM.insert "key2" (44::Int) mp  
--   return mp

