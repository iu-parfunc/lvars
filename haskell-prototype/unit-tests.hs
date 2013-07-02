{-# LANGUAGE TemplateHaskell, CPP, ScopedTypeVariables #-}

module Main where

import Test.Framework.Providers.HUnit 
import Test.Framework (Test, defaultMainWithArgs, testGroup)
import Test.Framework.TH (testGroupGenerator, defaultMainGenerator)

import Test.HUnit (Assertion, assertEqual, assertBool)
import qualified Test.HUnit as HU
import Control.Applicative
import Control.Concurrent
import Data.List (isInfixOf)
import qualified Data.Set as S
import System.Environment (getArgs)

import Control.Exception (catch, evaluate, SomeException)

import Data.Traversable (traverse)
import qualified Data.Set as S
import qualified Data.Map as M

import qualified Data.LVar.Set as IS
import qualified Data.LVar.Map as IM
import qualified Data.LVar.IVar as IV
import qualified Data.LVar.Pair as IP

import Control.LVish

import TestHelpers as T

--------------------------------------------------------------------------------


-- Disabling thread-variation due to below bug:
#if 0 
-- EEK!  Just got this [2013.06.27]:
-- 
-- unit-tests.exe: internal error: wakeup_gc_threads
--     (GHC version 7.6.3 for x86_64_unknown_linux)
--     Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug
-- Aborted (core dumped)

main :: IO ()
main = T.stdTestHarness $ return all_tests
 where 
 all_tests :: HU.Test
 all_tests =
   HU.TestList
   [
     HU.TestCase case_v0
   ]
   -- Ugh, busted test bracketing in test-framework... thus no good way to do
   -- thread-parameterization and no good way to take advantage of test-framework-th:   
   -- $(testGroupGenerator)
#else
main :: IO ()
main = $(defaultMainGenerator)
#endif


case_v0 :: HU.Assertion
case_v0 = do res <- v0
             HU.assertEqual "useless fork" (4::Int) res

v0 = runParIO $ do i <- IV.new; fork (return ()); IV.put i 4; IV.get i


case_v1 :: Assertion
case_v1 = assertEqual "fork put" (4::Int) =<< v1

v1 :: IO Int
v1 = runParIO $ do i<-IV.new; fork (IV.put i 4); IV.get i

case_v2a :: Assertion
case_v2a = v2a >>= assertEqual "put 10 in & wait"
          (S.fromList [1..10] :: S.Set Int)

-- [2013.06.27] getting thread-blocked-indefinitely errors:
v2a :: IO (S.Set Int)
v2a = runParIO $
     do s <- IS.newEmptySet
        mapM_ (\n -> fork $ IS.putInSet n s) [1..10]
        IS.waitSize 10 s 
        IS.freezeSet s

-- | This version uses a fork-join so it doesn't need the waitSize:
case_v2b :: Assertion
case_v2b = v2b >>= assertEqual "t2 with spawn instead of fork"
           (S.fromList [1..10] :: S.Set Int)
           
v2b :: IO (S.Set Int)
v2b = runParIO $
     do s <- IS.newEmptySet
        ivs <- mapM (\n -> IV.spawn_ $ IS.putInSet n s) [1..10]
        mapM_ IV.get ivs -- Join point.
        IS.freezeSet s


-- | Simple callback test.
case_v3a :: Assertion
case_v3a = v3a >>= assertEqual "simple callback test"
          (S.fromList [10,20,30,40,50,60,70,80,90,100] :: S.Set Int)

-- [2013.06.27] This is failing just occasionally with a multiple-put:
v3a :: IO (S.Set Int)          
v3a = runParIO $
     do s1 <- IS.newEmptySet
        s2 <- IS.newEmptySet
        let fn e = IS.putInSet (e*10) s2
        IS.withCallbacksThenFreeze s1 fn $ do
          -- Populate the first set:
          mapM_ (\n -> fork $ IS.putInSet n s1) [1..10]        
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
        let fn e = IS.putInSet (e*10) s2
        IS.withCallbacksThenFreeze s1 fn $ do
          -- Populate the first set:
          mapM_ (\n -> IS.putInSet n s1) [1..10]
          -- Because we filled s1 sequentially, we know it is full at this point.
          -- (If the above were forked we would need a finish/asnyc style construct)
          
        -- After all of s1's callbacks are finished executing, s2 is full:
        IS.freezeSet s2


-- | An under-synchronized test.  This should always return the same
-- result OR throw an exception.  In this case it should always return
-- a list of 10 elements, or throw an exception.
case_v3c :: Assertion
case_v3c = do 
  allowSomeExceptions ["Attempt to change a frozen LVar"] $ 
    do x <- v3c
       assertEqual "under-synchronized passed through"
      	           (S.fromList [10,20..100] :: S.Set Int) x
  return ()
    
v3c :: IO (S.Set Int)
v3c = runParIO $
     do s1 <- IS.newEmptySet
        s2 <- IS.newEmptySet
        let fn e = IS.putInSet (e*10) s2
        IS.withCallbacksThenFreeze s1 fn $ do
          mapM_ (\n -> fork $ IS.putInSet n s1) [1..10]          
          IS.waitSize 1 s2 -- Not ENOUGH synchronization!
          IS.freezeSet s2
          -- If this ^ freeze occurs *before* all the puts have happened,
          -- the a put happening after it will throw an exception.  If,
          -- on the other hand, it occurs after they've all happened,
          -- then we won't notice that anything is wrong and we'll get
          -- the same result we would have in case_v3.

-- FIXME: currently if run enough times, v3c can get the following failure:
-- I think we need to use full Async's so the cancellation goes both ways:

   -- Main:
   -- Exception inside child thread "worker thread", ThreadId 12: Attempt to change a frozen LVar
   -- Exception inside child thread "worker thread", ThreadId 9: Attempt to change a frozen LVar
   -- Exception inside child thread "worker thread", ThreadId 11: Attempt to change a frozen LVar
   -- test-lvish: Attempt to change a frozen LVar
   -- Exception inside child thread "worker thread", ThreadId 10: thread blocked indefinitely in an MVar operation

case_v7a :: Assertion
case_v7a = assertEqual "basic imap test"
           (M.fromList [(1,1.0),(2,2.0),(3,3.0),(100,100.1),(200,201.1)]) =<<
           v7a

v7a :: IO (M.Map Int Float)
v7a = runParIO $
  do mp <- IM.newEmptyMap
     fork $ do IM.waitSize 3 mp
               IM.insert 100 100.1 mp
     fork $ do IM.waitValue 100.1 mp
               v <- IM.getKey 1 mp
               IM.insert 200 (200.1 + v) mp
     IM.insert 1 1 mp
     IM.insert 2 2 mp
     liftIO$ putStrLn "[v7a] Did the first two puts.."
     liftIO$ threadDelay 1000
     IM.insert 3 3 mp
     liftIO$ putStrLn "[v7a] Did the first third put."
     IM.waitSize 5 mp
     IM.freezeMap mp


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
-- Do we need a "deep freeze" that freezes nested structures?
i7b = runParIO $ do
  mp <- IM.newEmptyMap
  s1 <- IS.newEmptySet
  s2 <- IS.newEmptySet
  IS.putInSet 0.11 s2
  f1 <- IV.spawn_ $ do IM.insert 1 s1 mp 
                       IM.insert 2 s2 mp
  f2 <- IV.spawn_ $ do s <- IM.getKey 1 mp
                       IS.putInSet 3.33 s
  -- RACE: this modify is racing with the insert of s2:
  IM.modify 2 mp (IS.putInSet 4.44)

  IV.get f1; IV.get f2
  mp2 <- IM.freezeMap mp
  traverse IS.freezeSet mp2

case_v7c :: Assertion
case_v7c = assertEqual "imap test - racing modifies"
           (M.fromList [(1,S.fromList [3.33]),
                        (2,S.fromList [4.44]),
                        (3,S.fromList [5.55,6.6])]) =<<
           v7c

-- | This example is valid because two modifies may race.
v7c :: IO (M.Map Int (S.Set Float))
-- Do we need a "deep freeze" that freezes nested structures?
v7c = runParIO $ do
  mp <- IM.newEmptyMap
  s1 <- IS.newEmptySet
  f1 <- IV.spawn_ $ IM.insert 1 s1 mp 
  f2 <- IV.spawn_ $ do s <- IM.getKey 1 mp
                       IS.putInSet 3.33 s
  IM.modify 2 mp (IS.putInSet 4.44)
  f3 <- IV.spawn_ $ IM.modify 3 mp (IS.putInSet 5.55)
  f4 <- IV.spawn_ $ IM.modify 3 mp (IS.putInSet 6.6)
  -- No easy way to wait on the total size of all contained sets...
  -- 
  -- Need a barrier here.. should have a monad-transformer that provides cilk "sync"
  -- Global quiesce is convenient too..
  IV.get f1; IV.get f2; IV.get f3; IV.get f4
  mp2 <- IM.freezeMap mp
  traverse IS.freezeSet mp2



--------------------------------------------------------------------------------
-- TEMPLATE HASKELL BUG? -- if we have *block* commented case_foo decls, it detects
-- those when it shouldn't:
--------------------------------------------------------------------------------

-- -- | Simple test of pairs.
-- case_v4 :: Assertion
-- case_v4 = v4 >>= assertEqual "simple-pair" (3, "hi") 

-- v4 :: IO (Int,String)
-- v4 = runParIO $
--      do p <- newPair
--         putFst p 3
--         putSnd p "hi"        
--         x <- getFst p
--         y <- getSnd p
--         return (x,y)

-- -- | This program should throw an exception due to multiple puts.
-- case_i5a :: Assertion
-- case_i5a = assertException ["Multiple puts to an IVar!"] i5a

-- i5a :: IO Int
-- i5a = runParIO (
--      do p <- newPair
--         putFst p 3
--         putSnd p "hi"
--         putSnd p "there"        
--         getFst p)

-- -- | Another exception due to multiple puts.  This tests whether the scheduler waits
-- -- around for a trailing (errorful) computation that is not on the main thread.
-- case_i5b :: Assertion
-- case_i5b = assertException ["Multiple puts to an IVar!"] i5b

-- i5b = 
--   runParIO $
--      do p <- newPair
--         putFst p 3
--         putSnd p "hi"
--         fork $ do waste_time
--                   putSnd p "there"
--         -- There's no 'consume' here; so we should really just get a
--         -- "Multiple puts to an IVar!" exception.
--         getSnd p

-- -- | Similar to 5b but with the branches flipped.
-- case_i5c :: Assertion
-- case_i5c = assertException ["Multiple puts to an IVar!"] i5c

-- i5c = runParIO $
--      do p <- newPair
--         putSnd p "hi"

--         -- The forked thread's value is not returned, so we go to a little extra work
--         -- here to bounce the value through the First of the pair.
--         fork $ putFst p =<< getSnd p
--         waste_time
        
--         putSnd p "there"
--         getFst p

-- -- | Another multiple put error.  This one makes sure that ANY tops get thrown as
-- -- exceptions, or we have full nondeterminism (not even limited guarantees), the
-- -- program would return "a" or "b".
-- case_i6a :: Assertion
-- case_i6a = assertException ["Multiple puts to an IVar!"] i6a
-- i6a = runParIO (
--      do p <- newPair
--         putFst p 3

--         -- TODO: Randomize these amounts of time:
--         fork $ do waste_time
--                   putSnd p "a"
--         fork $ do waste_time
--                   putSnd p "b"
--         -- There's no 'consume' here; so we should really just get a
--         -- "Multiple puts to an IVar!" exception.
--         getSnd p)


-- -- TODO:
-- --------------------------------
-- -- | This test, semantically, has two possible outcomes.  It can return "hi" or an
-- -- error.  That's quasi-determinism.  In practice, we force it to have one outcome by
-- -- wasting a significant amount of time in one branch.
-- --------------------------------


-- waste_time = loop 1000 3.3
--  where
--    loop 0 acc  = if acc < 10 then return acc else return 0
--    loop i !acc = loop (i - 1) (sin acc + 1.0)

-- -- More pairs
-- case_v6 :: Assertion
-- case_v6 = assertEqual "fancy pairs"
--           33 =<< runParIO (
--      do p1 <- newPair
--         p2 <- newPair
--         fork $ do x <- getFst p1
--                   putSnd p2 x 
--         fork $ do x <- getSnd p2
--                   putSnd p1 x
--         putFst p1 33
--         getSnd p1)


-- | Ensure that executing an action returns an exception
-- containing one of the expected messages.
assertException  :: [String] -> IO a -> IO ()
assertException msgs action = do
 x <- catch (do action; return Nothing) 
            (\e -> do putStrLn $ "Good.  Caught exception: " ++ show (e :: SomeException)
                      return (Just $ show e))
 case x of 
  Nothing -> error "Failed to get an exception!"
  Just s -> 
   if  any (`isInfixOf` s) msgs
   then return () 
   else error $ "Got the wrong exception, expected one of the strings: "++ show msgs
        ++ "\nInstead got this exception:\n  " ++ show s

-- | For testing quasi-deterministic programs: programs that always
-- either raise a particular exception or produce a particular answer.
allowSomeExceptions :: [String] -> IO a -> IO (Either SomeException a)
allowSomeExceptions msgs action = do
 catch (do a <- action; return (Right a))
       (\e ->
         let estr = show e in
         if  any (`isInfixOf` estr) msgs
          then do putStrLn $ "Caught allowed exception: " ++ show (e :: SomeException)
                  return (Left e)
          else error $ "Got the wrong exception, expected one of the strings: "++ show msgs
               ++ "\nInstead got this exception:\n  " ++ show estr)

assertOr :: Assertion -> Assertion -> Assertion
assertOr act1 act2 = 
  catch act1 
        (\(e::SomeException) -> act2)
