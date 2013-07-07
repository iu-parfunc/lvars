{-# LANGUAGE TemplateHaskell, CPP, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Test.Framework.Providers.HUnit 
import Test.Framework (Test, defaultMainWithArgs, testGroup)
import Test.Framework.TH (testGroupGenerator, defaultMainGenerator)

import Test.HUnit (Assertion, assertEqual, assertBool)
import qualified Test.HUnit as HU
import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Data.List (isInfixOf)
import qualified Data.Set as S
import System.Environment (getArgs)
import System.IO

import Control.Exception (catch, evaluate, SomeException)

import Data.Traversable (traverse)
import qualified Data.Set as S
import qualified Data.Map as M

import Data.LVar.Set as IS
import Data.LVar.Map as IM
import qualified Data.LVar.IVar as IV
import qualified Data.LVar.Pair as IP

import Control.LVish
import qualified Control.LVish.Internal as I
import Control.LVish.SchedIdempotent (liftIO)
import qualified Control.LVish.SchedIdempotent as L

import Data.Concurrent.SNZI as SNZI

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
     do s   <- IS.newEmptySet
        ivs <- mapM (\n -> IV.spawn_ $ IS.putInSet n s) [1..10]
        mapM_ IV.get ivs -- Join point.
        IS.freezeSet s

-- | This version uses deep freeze.
case_v2c :: Assertion
case_v2c = assertEqual "t2 with spawn instead of fork"
             (S.fromList [1..10] :: S.Set Int) v2c
v2c :: S.Set Int
v2c = runParThenFreeze $
     do s   <- IS.newEmptySet 
        ivs <- mapM (\n -> IV.spawn_ $ IS.putInSet n s) [1..10::Int]
        mapM_ IV.get ivs -- Join point.
        return s


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
            Nothing -> logStrLn $ "  [Invocation "++show elm++"] has no dependencies, running... "
            Just d -> do logStrLn $ "  [Invocation "++show elm++"] waiting on "++show dep
                         IS.waitElem d s2
                         logStrLn $ "  [Invocation "++show elm++"] dependency satisfied! "
          putInSet elm s2 
        logStrLn " [freezeSetAfter completed] "
        freezeSet s2

case_v3e :: Assertion
case_v3e = assertEqual "test of parallelism in addHandler"
              (S.fromList [1..5]) =<<  v3e

-- | Same as v3d but for addHandler
v3e :: IO (S.Set Int)
v3e = runParIO $ IS.freezeSet =<<
     do s1 <- IS.newFromList [1..5]
        s2 <- IS.newEmptySet
        hp <- newPool
        IS.addHandler hp s1 $ \ elm -> do
          let dep = case elm of
                      1 -> Just 2
                      2 -> Just 3
                      3 -> Nothing -- Foil either left-to-right or right-to-left
                      4 -> Just 3
                      5 -> Just 4
          case dep of
            Nothing -> logStrLn $ "  [Invocation "++show elm++"] has no dependencies, running... "
            Just d -> do logStrLn $ "  [Invocation "++show elm++"] waiting on "++show dep
                         IS.waitElem d s2
                         logStrLn $ "  [Invocation "++show elm++"] dependency satisfied! "
          putInSet elm s2
        quiesce hp
        logStrLn " [quiesce completed] "
        return s2


case_v7a :: Assertion
case_v7a = assertEqual "basic imap test"
           (M.fromList [(1,1.0),(2,2.0),(3,3.0),(100,100.1),(200,201.1)]) =<<
           v7a

v7a :: IO (M.Map Int Float)
v7a = runParIO $ IM.freezeMap =<<
  do mp <- IM.newEmptyMap
     fork $ do IM.waitSize 3 mp
               IM.insert 100 100.1 mp
     fork $ do IM.waitValue 100.1 mp
               v <- IM.getKey 1 mp
               IM.insert 200 (200.1 + v) mp
     IM.insert 1 1 mp
     IM.insert 2 2 mp
     logStrLn "[v7a] Did the first two puts.."
     I.liftIO$ threadDelay 1000
     IM.insert 3 3 mp
     logStrLn "[v7a] Did the first third put."
     IM.waitSize 5 mp
     return mp

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
  IM.modify mp 2 (IS.putInSet 4.44)

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
  IM.modify mp 2 (IS.putInSet 4.44)
  f3 <- IV.spawn_ $ IM.modify mp 3 (IS.putInSet 5.55)
  f4 <- IV.spawn_ $ IM.modify mp 3 (IS.putInSet 6.6)
  -- No easy way to wait on the total size of all contained sets...
  -- 
  -- Need a barrier here.. should have a monad-transformer that provides cilk "sync"
  -- Global quiesce is convenient too..
  IV.get f1; IV.get f2; IV.get f3; IV.get f4
  mp2 <- IM.freezeMap mp
  traverse IS.freezeSet mp2

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
  logStrLn " [v8a] now to construct cartesian product..."
  (h,s3) <- IS.cartesianProdHP Nothing s1 s2
  logStrLn " [v8a] cartesianProd call finished... next quiesce"
  IS.forEach s3 $ \ elm ->
    logStrLn$ " [v8a]   Got element: "++show elm
  putInSet 'c' s2
  quiesce h
  logStrLn " [v8a] quiesce finished, next freeze::"
  freezeSet s3

case_v8b :: Assertion
case_v8b = assertEqual "3-way cartesian product"
           (S.fromList
            [[1,40,101],[1,40,102],  [1,50,101],[1,50,102],
             [2,40,101],[2,40,102],  [2,50,101],[2,50,102]]
            )
           =<< v8b

-- [2013.07.03] Seeing nondeterministic failures here... hmm...
-- Ah, possibly divergence too... jeez.
v8b :: IO (S.Set [Int])
v8b = runParIO $ do
  hp <- newPool
  s1 <- IS.newFromList [1,2]
  s2 <- IS.newFromList [40,50]
    -- (hp,s3) <- IS.traverseSetHP Nothing (return . (+100)) s1
  (_,s3) <- IS.traverseSetHP    (Just hp) (return . (+100)) s1
  (_,s4) <- IS.cartesianProdsHP (Just hp) [s1,s2,s3]
  IS.forEachHP (Just hp) s4 $ \ elm ->
    logStrLn $ " [v8b]   Got element: "++show elm
  -- [2013.07.03] Confirmed: this makes the bug(s) go away:  
  -- liftIO$ threadDelay$ 100*1000
  quiesce hp
  logStrLn " [v8b] quiesce finished, next freeze::"
  freezeSet s4



-- v8b :: IO (S.Set Int)
-- v8b = runParIO $ do
--   s1 <- IS.newFromList [1,2,3]
--   s2 <- IS.newFromList [2,3,4]
--   s3 <- IS.intersection s1 s2
--   quiesce h
--   freezeSet s3
  
--------------------------------------------------------------------------------
-- TESTS FOR SNZI  
--------------------------------------------------------------------------------
  
-- | Test snzi in a sequential setting
snzi1 :: IO (Bool)
snzi1 = do
  (cs, poll) <- SNZI.newSNZI
  forM_ cs SNZI.arrive  
  forM_ cs SNZI.arrive
  forM_ cs SNZI.depart  
  forM_ cs SNZI.depart
  poll
  
case_snzi1 :: Assertion  
case_snzi1 = snzi1 >>= assertEqual "sequential use of SNZI" True

-- | Very simple sequential snzi test
snzi2a :: IO (Bool)
snzi2a = do
  (cs, poll) <- SNZI.newSNZI
  forM_ cs SNZI.arrive  
  poll
  
case_snzi2a :: Assertion  
case_snzi2a = snzi2a >>= assertEqual "sequential use of SNZI" False

-- | Test snzi in a sequential setting
snzi2 :: IO (Bool)
snzi2 = do
  (cs, poll) <- SNZI.newSNZI
  forM_ cs SNZI.arrive  
  forM_ cs SNZI.arrive
  forM_ cs SNZI.depart  
  forM_ cs SNZI.depart
  forM_ cs SNZI.arrive
  poll
  
case_snzi2 :: Assertion  
case_snzi2 = snzi2 >>= assertEqual "sequential use of SNZI" False

nTimes :: Int -> IO () -> IO ()
nTimes 0 _ = return ()
nTimes n c = c >> nTimes (n-1) c

-- | Test snzi in a concurrent setting
snzi3 :: IO (Bool)
snzi3 = do
  (cs, poll) <- SNZI.newSNZI
  mvars <- forM cs $ \c -> do
    mv <- newEmptyMVar
    forkIO $ do 
      nTimes 1000000 $ do
        SNZI.arrive c
        SNZI.depart c
        SNZI.arrive c
        SNZI.arrive c
        SNZI.depart c
        SNZI.depart c
      putMVar mv ()
    return mv
  forM_ mvars takeMVar
  poll
  
case_snzi3 :: Assertion  
case_snzi3 = snzi3 >>= assertEqual "concurrent use of SNZI" True

-- | Test snzi in a concurrent setting
snzi4 :: IO (Bool)
snzi4 = do
  (cs, poll) <- SNZI.newSNZI
  mvars <- forM cs $ \c -> do
    mv <- newEmptyMVar
    internalMV <- newEmptyMVar
    forkIO $ do
      SNZI.arrive c
      putMVar internalMV ()
    forkIO $ do 
      nTimes 1000000 $ do
        SNZI.arrive c
        SNZI.depart c
        SNZI.arrive c
        SNZI.arrive c
        SNZI.depart c
        SNZI.depart c
      takeMVar internalMV
      putMVar mv ()
    return mv
  forM_ mvars takeMVar
  poll
  
case_snzi4 :: Assertion  
case_snzi4 = snzi4 >>= assertEqual "concurrent use of SNZI" False

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


--------------------------------------------------------------------------------
-- Unit Tests
--------------------------------------------------------------------------------

case_dftest0 = assertEqual "manual freeze, outer layer" "hello" =<< dftest0

dftest0 :: IO String
dftest0 = runParIO $ do
  iv1 <- newBottom -- :: Par d s (IV.IVar s (IV.IVar s String))
  iv2 <- newBottom
  IV.put_ iv1 iv2
  IV.put_ iv2 "hello"
  IV.IVarSnap m <- freeze iv1
  case m of
    Just i -> IV.get i

case_dftest1 = assertEqual "deefreeze double ivar" (Just (Just "hello")) =<< dftest1

-- | Should return (Just (Just "hello"))
dftest1 :: IO(Maybe (Maybe String))
dftest1 = runParIO $ do
  iv1 <- newBottom -- :: Par QuasiDet s (IV.IVar (IV.IVar String))
  iv2 <- newBottom
  IV.put_ iv1 iv2
  IV.put_ iv2 "hello"
  deepFreeze iv1

case_dftest2 = assertEqual "deefreeze double ivar"
                 (IV.IVarSnap (Just (IV.IVarSnap (Just "hello")))) =<< dftest2

-- | This uses the more generic lifting... but it's more annoying to unpack:
dftest2 :: IO (Snapshot IV.IVar (Snapshot IV.IVar String))
dftest2 = runParIO $ do
  iv1 <- newBottom --- :: Par (IV.IVar s (IV.IVar s String))
  iv2 <- newBottom
  IV.put_ iv1 iv2
  IV.put_ iv2 "hello"
  deepFreeze iv1


dftest3 :: IO (Maybe Int)
dftest3 = runParIO $ do
  iv1 <- newBottom 
  IV.put_ iv1 (3::Int)
  deepFreeze iv1 

-- | Polymorphic version of previous
{-
dftest4 :: DeepFreeze (IV.IVar s Int) b => IO b
dftest4 = runParIO $ do
  iv1::IV.IVar s1 Int <- newBottom 
  IV.put_ iv1 (3::Int)
--  res <- deepFreeze iv1 :: Par QuasiDet (Session (IV.IVar s1 Int)) b
  res <- deepFreeze iv1 :: Par QuasiDet s1 b
  return res

ase_dftest4a = assertEqual "freeze polymorphic 1"
                  (IV.IVarSnap (Just 3)) =<< dftest4a

-- More flexible than regular freeze, can pick either type:
dftest4a :: IO (Snapshot IV.IVar Int)
dftest4a = dftest4

ase_dftest4b = assertEqual "freeze polymorphic 2"
                  (Just 3) =<< dftest4b

-- uh... how is this one actually working?
dftest4b :: IO (Maybe Int)
dftest4b = dftest4
-}

------------------------------------------------------------------------------------------

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
 catch (do a <- action; evaluate a; return (Right a))
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
