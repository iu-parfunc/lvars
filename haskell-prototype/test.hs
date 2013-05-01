{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

#if defined(LVARPURE)
#warning "Using the LVar Pure version"
import LVarTracePure
import Data.LVar.PairPure
import Data.LVar.SetPure
#elif defined(LVARIO)
#warning "Using the LVar IO version"
import LVarTraceIO
import Data.LVar.PairIO
import Data.LVar.SetIO
#elif defined(LVARSCALABLE)
#warning "Using the LVar Scalable version"
import LVarTraceScalable
import Data.LVar.PairScalable
import Data.LVar.SetScalable
#endif

import Prelude hiding (catch)
import Control.Exception (catch, evaluate, SomeException)
import Control.Concurrent (setNumCapabilities)
import Data.List (isInfixOf)
import qualified Data.Set as S
import Test.HUnit (Assertion, assertEqual, assertBool)
import Test.Framework.Providers.HUnit 
import Test.Framework (Test, defaultMainWithArgs, testGroup)
import Test.Framework.TH (testGroupGenerator)
import System.Environment (getArgs)

main :: IO ()
main = do 
  args <- getArgs
  -- Set the number of threads on the command line: Here we snatch an
  -- argument before test-framework gets to it.
  let (threads,args') = 
         case concatMap reads args of 
           (x,_):_ -> (x, tail args)
           _       -> (4, args)
  putStrLn$ "Setting the number of threads to: "++show threads
  setNumCapabilities threads
  -- Template haskell magic to aggregate the tests from this file:
  let alltests :: Test
      alltests = $(testGroupGenerator)
  defaultMainWithArgs [alltests] args'
--  defaultMainWithArgs (testGroup "LVar tests"  alltests) args'

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


-- TODO:
assertOr :: Assertion -> Assertion -> Assertion
assertOr = error "TODO: Disjunction over assertions..." 

--------------------------------------------------------------------------------

case_v0 :: Assertion
case_v0 = assertEqual "useless fork" (4::Int) v0
          
v0 = runPar $ do i<-new; fork (return ()); put i 4; get i

case_v1 :: Assertion
case_v1 = assertEqual "fork put" (4::Int) v1

v1 = runPar $ do i<-new; fork (put i 4); get i

case_v2 :: Assertion
case_v2 = v2 >>= assertEqual "put 10 in & wait"
          (S.fromList [1..10] :: S.Set Int)
          
v2 :: IO (S.Set Int)          
v2 = runParIO $
     do s <- newEmptySet
        mapM_ (\n -> fork $ putInSet n s) [1..10]
        waitForSetSize 10 s 
        consumeSet s

-- | This version uses a fork-join so it doesn't need the waitForSetSize:
case_v2b :: Assertion
case_v2b = v2b >>= assertEqual "t2 with spawn instead of fork"
           (S.fromList [1..10] :: S.Set Int)
           
v2b :: IO (S.Set Int)
v2b = runParIO $
     do s <- newEmptySet
        ivs <- mapM (\n -> spawn_ $ putInSet n s) [1..10]
        mapM_ get ivs -- Join point.
        consumeSet s

-- | Simple callback test.
case_v3 :: Assertion
case_v3 = v3 >>= assertEqual "simple callback test"
          (S.fromList [10,20,30,40,50,60,70,80,90,100] :: S.Set Int)
          
v3 :: IO (S.Set Int)          
v3 = runParIO $
     do s1 <- newEmptySet
        let fn e = putInSet (e*10) s1 
            
        s2 <- newEmptySetWithCallBack fn
        mapM_ (\n -> fork $ putInSet n s2) [1..10]
        
        -- We never read out of s2 directly.  Instead, writes to s2
        -- trigger the callback 'fn' to run, with the element written
        -- to s2 as their argument.  So eventually, ten elements are
        -- written to s1.
        waitForSetSize 10 s1
        consumeSet s1

-- | An under-synchronized test.  This should always return the same
-- result OR throw an exception.  In this case it should always return
-- a list of 10 elements, or throw an exception.
case_v3b :: Assertion
case_v3b = v3b >>= assertEqual "under-synchronized"
           (S.fromList [10,20,30,40,50,60,70,80,90,100] :: S.Set Int)
          
v3b :: IO (S.Set Int)
v3b = runParIO $
     do s1 <- newEmptySet
        let fn e = putInSet (e*10) s1 
        
        s2 <- newEmptySetWithCallBack fn
        mapM_ (\n -> fork$ putInSet n s2) [1..10]
        
        waitForSetSize 1 s1
        
        -- If this consume occurs before all the puts have happened,
        -- the a put happening after it will throw an exception.  If,
        -- on the other hand, it occurs after they've all happened,
        -- then we won't notice that anything is wrong and we'll get
        -- the same result we would have in case_v3.
        consumeSet s1

-- | Simple test of pairs.
case_v4 :: Assertion
case_v4 = v4 >>= assertEqual "simple-pair" (3, "hi") 

v4 :: IO (Int,String)
v4 = runParIO $
     do p <- newPair
        putFst p 3
        putSnd p "hi"        
        x <- getFst p
        y <- getSnd p
        return (x,y)

-- | This program should throw an exception due to multiple puts.
case_i5a :: Assertion
case_i5a = assertException ["Multiple puts to an IVar!"] i5a

i5a :: IO Int
i5a = runParIO (
     do p <- newPair
        putFst p 3
        putSnd p "hi"
        putSnd p "there"        
        getFst p)

-- | Another exception due to multiple puts.  This tests whether the scheduler waits
-- around for a trailing (errorful) computation that is not on the main thread.
case_i5b :: Assertion
case_i5b = assertException ["Multiple puts to an IVar!"] i5b

i5b = 
  runParIO $
     do p <- newPair
        putFst p 3
        putSnd p "hi"
        fork $ do waste_time
                  putSnd p "there"
        -- There's no 'consume' here; so we should really just get a
        -- "Multiple puts to an IVar!" exception.
        getSnd p

-- | Similar to 5b but with the branches flipped.
case_i5c :: Assertion
case_i5c = assertException ["Multiple puts to an IVar!"] i5c

i5c = runParIO $
     do p <- newPair
        putSnd p "hi"

        -- The forked thread's value is not returned, so we go to a little extra work
        -- here to bounce the value through the First of the pair.
        fork $ putFst p =<< getSnd p
        waste_time
        
        putSnd p "there"
        getFst p

-- | Another multiple put error.  This one makes sure that ANY tops get thrown as
-- exceptions, or we have full nondeterminism (not even limited guarantees), the
-- program would return "a" or "b".
case_i6a :: Assertion
case_i6a = assertException ["Multiple puts to an IVar!"] i6a
i6a = runParIO (
     do p <- newPair
        putFst p 3

        -- TODO: Randomize these amounts of time:
        fork $ do waste_time
                  putSnd p "a"
        fork $ do waste_time
                  putSnd p "b"
        -- There's no 'consume' here; so we should really just get a
        -- "Multiple puts to an IVar!" exception.
        getSnd p)


-- TODO:
--------------------------------
-- | This test, semantically, has two possible outcomes.  It can return "hi" or an
-- error.  That's quasi-determinism.  In practice, we force it to have one outcome by
-- wasting a significant amount of time in one branch.
--------------------------------


waste_time = loop 1000 3.3
 where
   loop 0 acc  = if acc < 10 then return acc else return 0
   loop i !acc = loop (i - 1) (sin acc + 1.0)

-- More pairs
case_v6 :: Assertion
case_v6 = assertEqual "fancy pairs"
          33 =<< runParIO (
     do p1 <- newPair
        p2 <- newPair
        fork $ do x <- getFst p1
                  putSnd p2 x 
        fork $ do x <- getSnd p2
                  putSnd p1 x
        putFst p1 33
        getSnd p1)

