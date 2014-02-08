{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | Tests for the Data.LVar.PureSet and Data.LVar.SLSet modules.

module SetTests(tests, runTests) where

import Test.Framework.Providers.HUnit 
import Test.Framework (Test, defaultMain, testGroup)
import Test.HUnit (Assertion, assertEqual, assertBool, Counts(..))
import Test.Framework.TH (testGroupGenerator)
import qualified Test.HUnit as HU
import           TestHelpers as T

import qualified Data.Set as S

import qualified Data.LVar.Generic as G
import Data.LVar.PureSet as IS
import qualified Data.LVar.SLSet as SS
import qualified Data.LVar.IVar as IV

import           Control.LVish
import           Control.LVish.DeepFrz (DeepFrz(..), Frzn, Trvrsbl, runParThenFreeze, runParThenFreezeIO)

--------------------------------------------------------------------------------

tests :: Test
tests = $(testGroupGenerator)

runTests :: IO ()
runTests = defaultMainSeqTests [tests]

--------------------------------------------------------------------------------

case_v2a :: Assertion
case_v2a = v2a >>= assertEqual "put 10 in & wait"
          (S.fromList [1..10] :: S.Set Int)

-- [2013.06.27] getting thread-blocked-indefinitely errors:
v2a :: IO (S.Set Int)
v2a = runParIO $
     do s <- IS.newEmptySet
        mapM_ (\n -> fork $ IS.insert n s) [1..10]
        IS.waitSize 10 s 
        IS.freezeSet s

-- | This version uses a fork-join so it doesn't need the waitSize:
case_v2b :: Assertion
case_v2b = v2b >>= assertEqual "t2 with spawn instead of fork"
           (S.fromList [1..10] :: S.Set Int)
           
v2b :: IO (S.Set Int)
v2b = runParIO $
     do s   <- IS.newEmptySet
        ivs <- mapM (\n -> IV.spawn_ $ IS.insert n s) [1..10]
        mapM_ IV.get ivs -- Join point.
        IS.freezeSet s

-- | This version uses deep freeze.        
case_v2c :: Assertion
case_v2c = assertEqual "t2 with spawn instead of fork"
             (S.fromList [1..10] :: S.Set Int)
             (IS.fromISet v2c)
             
-- v2c :: S.Set Int
v2c :: IS.ISet Frzn Int
v2c = -- IS.fromISet $
      runParThenFreeze par
  where
    par :: Deterministic e => Par e s (IS.ISet s Int)
    par = 
     do s   <- IS.newEmptySet 
        ivs <- mapM (\n -> IV.spawn_ $ IS.insert n s) [1..10::Int]
        mapM_ IV.get ivs -- Join point.
        return s

-- [2013.06.27] This is failing just occasionally with a multiple-put:
v3a :: IO (S.Set Int)          
v3a = runParIO $
     do s1 <- IS.newEmptySet
        s2 <- IS.newEmptySet
        let fn e = IS.insert (e*10) s2
        IS.withCallbacksThenFreeze s1 fn $ do
          -- Populate the first set:
          mapM_ (\n -> fork $ IS.insert n s1) [1..10]        
          -- We never read out of s1 directly.  Instead, writes to s1 trigger the
          -- callback 'fn' to run, with the element written to s2.  So eventually,
          -- ten elements are written to s2.
          IS.waitSize 10 s2
          IS.freezeSet s2

case_v3b :: Assertion
case_v3b = v3b >>= assertEqual "simple callback test"
          (S.fromList [10,20,30,40,50,60,70,80,90,100] :: S.Set Int)
          
v3b :: IO (S.Set Int)          
v3b = runParIO $
     do s1 <- IS.newEmptySet
        s2 <- IS.newEmptySet
        let fn e = IS.insert (e*10) s2
        IS.withCallbacksThenFreeze s1 fn $ do
          -- Populate the first set:
          mapM_ (\n -> IS.insert n s1) [1..10]
          -- Because we filled s1 sequentially, we know it is full at this point.
          -- (If the above were forked we would need a finish/asnyc style construct)
          
        -- After all of s1's callbacks are finished executing, s2 is full:
        IS.freezeSet s2


-- | An under-synchronized test.  This should always return the same
-- result OR throw an exception.  In this case it should always return
-- a list of 10 elements, or throw an exception.
case_i3c :: Assertion
case_i3c = do 
  allowSomeExceptions ["Attempt to change a frozen LVar"] $ 
    do x <- i3c
       assertEqual "under-synchronized passed through"
      	           (S.fromList [10,20..100] :: S.Set Int) x
  return ()
    
i3c :: IO (S.Set Int)
i3c = runParIO $
     do s1 <- IS.newEmptySet
        s2 <- IS.newEmptySet
        let fn e = IS.insert (e*10) s2
        IS.withCallbacksThenFreeze s1 fn $ do
          mapM_ (\n -> fork $ IS.insert n s1) [1..10]          
          IS.waitSize 1 s2 -- Not ENOUGH synchronization!
          IS.freezeSet s2
          -- If this ^ freeze occurs *before* all the puts have happened,
          -- the a put happening after it will throw an exception.  If,
          -- on the other hand, it occurs after they've all happened,
          -- then we won't notice that anything is wrong and we'll get
          -- the same result we would have in case_v3.

-- FIXME: currently if run enough times, i3c can get the following failure:
-- I think we need to use full Async's so the cancellation goes both ways:

   -- Main:
   -- Exception inside child thread "worker thread", ThreadId 12: Attempt to change a frozen LVar
   -- Exception inside child thread "worker thread", ThreadId 9: Attempt to change a frozen LVar
   -- Exception inside child thread "worker thread", ThreadId 11: Attempt to change a frozen LVar
   -- test-lvish: Attempt to change a frozen LVar
   -- Exception inside child thread "worker thread", ThreadId 10: thread blocked indefinitely in an MVar operation


case_v3d :: Assertion
case_v3d = assertEqual "test of parallelism in freezeSetAfter"
              (S.fromList [1..5]) =<<  v3d

-- | This test has interdependencies between callbacks (that are launched on
-- already-present data), which forces these to be handled in parallel.
v3d :: IO (S.Set Int)
v3d = runParIO $ 
     do s1 <- IS.newFromList [1..5]
        s2 <- IS.newEmptySet
        IS.freezeSetAfter s1 $ \ elm -> do
          let dep = case elm of
                      1 -> Just 2
                      2 -> Just 3
                      3 -> Nothing -- Foil either left-to-right or right-to-left
                      4 -> Just 3
                      5 -> Just 4
          case dep of
            Nothing -> logDbgLn 1 $ "  [Invocation "++show elm++"] has no dependencies, running... "
            Just d -> do logDbgLn 1 $ "  [Invocation "++show elm++"] waiting on "++show dep
                         IS.waitElem d s2
                         logDbgLn 1 $ "  [Invocation "++show elm++"] dependency satisfied! "
          IS.insert elm s2 
        logDbgLn 1 " [freezeSetAfter completed] "
        freezeSet s2

case_v3e :: Assertion
case_v3e = assertEqual "test of parallelism in forEachHP"
              (S.fromList [1..5]) =<<  v3e

-- | Same as v3d but for forEachHP
v3e :: IO (S.Set Int)
v3e = runParIO $ IS.freezeSet =<<
     do s1 <- IS.newFromList [1..5]
        s2 <- IS.newEmptySet
        hp <- newPool
        IS.forEachHP (Just hp) s1 $ \ elm -> do
          let dep = case elm of
                      1 -> Just 2
                      2 -> Just 3
                      3 -> Nothing -- Foil either left-to-right or right-to-left
                      4 -> Just 3
                      5 -> Just 4
          case dep of
            Nothing -> logDbgLn 1 $ "  [Invocation "++show elm++"] has no dependencies, running... "
            Just d -> do logDbgLn 1 $ "  [Invocation "++show elm++"] waiting on "++show dep
                         IS.waitElem d s2
                         logDbgLn 1 $ "  [Invocation "++show elm++"] dependency satisfied! "
          IS.insert elm s2
        quiesce hp
        logDbgLn 1 " [quiesce completed] "
        return s2

--------------------------------------------------------------------------------
-- Higher level derived ops
--------------------------------------------------------------------------------  

case_v8a :: Assertion
case_v8a = assertEqual "simple cartesian product test"
           (S.fromList
            [(1,'a'),(1,'b'),(1,'c'),
             (2,'a'),(2,'b'),(2,'c'),
             (3,'a'),(3,'b'),(3,'c')])
           =<< v8a

-- v8a :: IO (S.Set (Integer, Char))
v8a :: IO (S.Set (Integer, Char))
v8a = runParIO $ do
  s1 <- IS.newFromList [1,2,3]
  s2 <- IS.newFromList ['a','b']
  logDbgLn 1 " [v8a] now to construct cartesian product..."
  h  <- newPool
  s3 <- IS.cartesianProdHP (Just h) s1 s2
  logDbgLn 1 " [v8a] cartesianProd call finished... next quiesce"
  IS.forEach s3 $ \ elm ->
    logDbgLn 1$ " [v8a]   Got element: "++show elm
  IS.insert 'c' s2
  quiesce h
  logDbgLn 1 " [v8a] quiesce finished, next freeze::"
  freezeSet s3

case_v8b :: Assertion
case_v8b = assertEqual "3-way cartesian product"
           (S.fromList
            [[1,40,101],[1,40,102],  [1,50,101],[1,50,102],
             [2,40,101],[2,40,102],  [2,50,101],[2,50,102]]
            )
           =<< v8b

v8b :: IO (S.Set [Int])
v8b = runParIO $ do
  hp <- newPool
  s1 <- IS.newFromList [1,2]
  s2 <- IS.newFromList [40,50]
    -- (hp,s3) <- IS.traverseSetHP Nothing (return . (+100)) s1
  s3 <- IS.traverseSetHP    (Just hp) (return . (+100)) s1
  s4 <- IS.cartesianProdsHP (Just hp) [s1,s2,s3]
  IS.forEachHP (Just hp) s4 $ \ elm ->
    logDbgLn 1 $ " [v8b]   Got element: "++show elm
  -- [2013.07.03] Confirmed: this makes the bug(s) go away:  
  -- liftIO$ threadDelay$ 100*1000
  quiesce hp
  logDbgLn 1 " [v8b] quiesce finished, next freeze::"
  freezeSet s4

------------------------------------------------------------------------------------------
-- Show instances
------------------------------------------------------------------------------------------

case_show05 :: Assertion
case_show05 = assertEqual "show for PureSet" "{ISet: 33, 44}" (show show05)
show05 :: ISet Frzn Int
show05 = runParThenFreeze $ do
  is <- IS.newEmptySet
  IS.insert (33::Int) is
  IS.insert (44::Int) is
  return is

-- | It happens that these come out in the opposite order from the Pure one:
case_show06 :: Assertion
case_show06 = assertEqual "show for SLSet" "{ISet: 44, 33}" (show show06)
show06 :: SS.ISet Frzn Int
show06 = runParThenFreeze $ do
  is <- SS.newEmptySet
  SS.insert (33::Int) is
  SS.insert (44::Int) is
  return is

----------------------------------------
-- Test sortFrzn instances:

case_show05B :: Assertion
case_show05B = assertEqual "show for PureSet/Trvrsbl" "AFoldable [33, 44]" (show show05B)
show05B :: G.AFoldable Int
show05B = G.sortFrzn show05

case_show06B :: Assertion
case_show06B = assertEqual "show for SLSet/Trvrsbl" "AFoldable [44, 33]" (show show06B)
show06B :: G.AFoldable Int
show06B = G.sortFrzn show06
