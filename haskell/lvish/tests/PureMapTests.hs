{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}

-- | Tests for the Data.LVar.PureMap and Data.LVar.SLMap modules.

module PureMapTests(tests, runTests) where

import Data.LVar.PureSet as IS
import qualified Data.LVar.PureMap as IM 
  -- The common interface under test:
  (IMap, waitSize, waitValue, getKey, insert, newEmptyMap, newFromList, 
   freezeMap, unionHP, forEach, forEachHP, traverseMap, traverseMapHP, modify)

-- TODO: Use backpack for this when it is available:
#include "CommonMapTests.hs"

type TheMap k s v = IM.IMap k s v 

--------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "" [tests_here, tests_common ]

tests_here :: TestTree
tests_here = $(testGroupGenerator)

runTests :: IO ()
runTests = defaultMain tests

--------------------------------------------------------------------------------

-- [2013.08.05] RRN: Observing nondeterministic blocked-indefinitely
-- exception here.
case_i7b :: Assertion
case_i7b = do 
  allowSomeExceptions ["Multiple puts"] $ 
    assertEqual "racing insert and modify"
                 (M.fromList [(1,S.fromList [3.33]),
                              (2,S.fromList [0.11,4.44])]) =<<
                i7b
  return ()

-- | A quasi-deterministic example.
i7b :: IO (M.Map Int (S.Set Float))
-- A manual nested freeze instead of DeepFrz:
i7b = runParQuasiDet $ do
  mp <- IM.newEmptyMap
  s1 <- IS.newEmptySet
  s2 <- IS.newEmptySet
  IS.insert 0.11 s2
  f1 <- IV.spawn_ $ do IM.insert 1 s1 mp 
                       IM.insert 2 s2 mp
  f2 <- IV.spawn_ $ do s <- IM.getKey 1 mp
                       IS.insert 3.33 s
  -- RACE: this modify is racing with the insert of s2:
  IM.modify mp 2 IS.newEmptySet (IS.insert 4.44) 

  IV.get f1; IV.get f2
  mp2 <- IM.freezeMap mp
  traverse IS.freezeSet mp2

-- | This example is valid because two modifies may race.
v7c :: IO (M.Map Int (S.Set Float))
-- Do we need a "deep freeze" that freezes nested structures?
v7c = runParQuasiDet $ do
  mp <- IM.newEmptyMap
  s1 <- IS.newEmptySet
  f1 <- IV.spawn_ $ IM.insert 1 s1 mp 
  f2 <- IV.spawn_ $ do s <- IM.getKey 1 mp
                       IS.insert 3.33 s
  IM.modify mp 2 IS.newEmptySet (IS.insert 4.44)
  f3 <- IV.spawn_ $ IM.modify mp 3 IS.newEmptySet (IS.insert 5.55)
  f4 <- IV.spawn_ $ IM.modify mp 3 IS.newEmptySet (IS.insert 6.6)
  -- No easy way to wait on the total size of all contained sets...
  -- 
  -- Need a barrier here.. should have a monad-transformer that provides cilk "sync"
  -- Global quiesce is convenient too..
  IV.get f1; IV.get f2; IV.get f3; IV.get f4
  mp2 <- IM.freezeMap mp
  traverse IS.freezeSet mp2

case_v7c :: Assertion
case_v7c = assertEqual "imap test - racing modifies"
           (M.fromList [(1,S.fromList [3.33]),
                        (2,S.fromList [4.44]),
                        (3,S.fromList [5.55,6.6])]) =<<
           v7c

------------------------------------------------------------------------------------------
-- Show instances
------------------------------------------------------------------------------------------

case_show03 :: Assertion
case_show03 = assertEqual "show for PureMap" "{IMap: (\"key1\",33), (\"key2\",44)}" show03
show03 :: String
show03 = show$ runParThenFreeze $ do
  mp <- IM.newEmptyMap
  IM.insert "key1" (33::Int) mp
  IM.insert "key2" (44::Int) mp  
  return mp


