{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

-- | Tests for the Data.LVar.PureMap and Data.LVar.SLMap modules.

module PureMapTests(tests, runTests) where

import Data.LVar.PureSet as IS
import Data.LVar.PureMap as IM

#include "CommonMapTests.hs"

--------------------------------------------------------------------------------

tests :: Test
tests = testGroup "" [tests_here, tests_common ]

tests_here :: Test
tests_here = $(testGroupGenerator)

runTests :: IO ()
runTests = defaultMain [tests]

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
i7b = runParIO $ do
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
v7c = runParIO $ do
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

--------------------------------------------------------------------------------
-- Tests that use `forEachHP`
--------------------------------------------------------------------------------  

case_v8c :: Assertion
case_v8c = assertEqual "forEachHP on maps"
           (M.fromList [(1,101),(2,102)] ) =<< v8c

-- | Similar test with Maps instead of Sets.
v8c :: IO (M.Map Int Int)
v8c = runParIO $ do
  hp <- newPool
  m1 <- IM.newFromList [(1,1),(2,2)]
  m2 <- newEmptyMap
  let cb k v = do logDbgLn 1$" [v8c]  Inside callback for Map.. key="++show k
                  IM.insert k (v+100) m2
  IM.forEachHP (Just hp) m1 cb 
  logDbgLn 1 " [v8c] Everything set up; about to quiesce..."
  quiesce hp
  logDbgLn 1 " [v8c] quiesce finished, next freeze:"
  freezeMap m2


case_v8d :: Assertion
case_v8d = assertEqual "union on maps"
--           (M.fromList [(1,101),(2,102),(40,40),(50,50)] )
           [40,50,101,102] =<< v8d
-- v8d :: IO (M.Map Int Int)
v8d :: IO [Int]
v8d = fmap (L.sort . F.toList) $
      runParIO $ do
  hp <- newPool
  logDbgLn 1 " [v8d] Got a new pool..."  
  m1 <- IM.newFromList [(1,1),(2,2)]
  m2 <- IM.newFromList [(40,40),(50,50)]
  logDbgLn 1 " [v8d] Got two fresh maps..."
  let cb k v = do logDbgLn 1$" [v8d]  Inside callback for traverse.. key="++show k
                  return (v+100)
  m3 <- IM.traverseMapHP (Just hp) cb m1
  m4 <- IM.unionHP       (Just hp) m2 m3
  IM.forEachHP (Just hp) m4 $ \ k elm ->
    logDbgLn 1 $ " [v8d]   Got element: "++show (k,elm)
  logDbgLn 1 " [v8d] Everything set up; about to quiesce..."
  quiesce hp
--  quiesceAll  
  logDbgLn 1 " [v8d] quiesce finished, next freeze::"
  IM.freezeMap m4

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


