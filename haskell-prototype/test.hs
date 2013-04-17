{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

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
-- This version definitely doesn't work yet. -- LK
#warning "Using the LVar Scalable version"
import LVarTraceScalable
import Data.LVar.PairScalable
import Data.LVar.SetScalable
#endif

import Data.Set as S
import Test.HUnit (Assertion, assertEqual, assertBool)
import Test.Framework.Providers.HUnit
import Test.Framework (Test)
--import Test.Framework.TH (defaultMainGenerator)

main :: IO ()
--main = $(defaultMainGenerator)

-- Temporary until I can figure out why Test.Framework.TH doesn't work
-- for me. -- LK
main = do
  case_t0
  case_t1
  case_t2
  case_t2b
  t3
  t4
  t5
  t6
  return ()
  
--------------------------------------------------------------------------------

case_t0 :: Assertion
case_t0 = assertEqual "useless fork" (4::Int) $ 
          runPar$ do i<-new; fork (return ()); put i 4; get i

case_t1 :: Assertion
case_t1 = assertEqual "fork put" (4::Int) $
          runPar$ do i<-new; fork (put i 4); get i

case_t2 :: Assertion -- IO (S.Set Int)
case_t2 = assertEqual "put 10 in & wait"
          (S.fromList [1..10] :: S.Set Int) =<< runParIO (
     do s <- newEmptySet
        mapM_ (\n -> fork$ putInSet n s) [1..10]
        waitForSetSize 10 s 
        consumeSet s)

-- | This version uses a fork-join so it doesn't need the waitForSetSize:
case_t2b :: Assertion
case_t2b = assertEqual "t2 with spawn instead of fork"
           (S.fromList [1..10] :: S.Set Int) =<< runParIO (
     do s <- newEmptySet
        ivs <- mapM (\n -> spawn_$ putInSet n s) [1..10]
        mapM_ get ivs -- Join point.
        consumeSet s)

-- | Simple callback test.
t3 :: IO (S.Set Int)
t3 = runParIO $
     do s1 <- newEmptySet
        let fn e = putInSet (e*10) s1 
        s2 <- newEmptySetWithCallBack fn

        mapM_ (\n -> fork$ putInSet n s2) [1..10]
        waitForSetSize 10 s1
        consumeSet s1


-- | Simple test of pairs.
t4 = runParIO $
     do p <- newPair
        putFst p 3
        putSnd p "hi"        
        x <- getFst p
        y <- getSnd p
        return (x,y)

-- | This seems pretty naughty, but for now it works!
t5 = runParIO $
     do p <- newPair
        putFst p 3
        putSnd p "hi"
        putSnd p "there"        
        getFst p

-- More pairs
t6 = runParIO $
     do p1 <- newPair
        p2 <- newPair
        fork$ do x <- getFst p1
                 putSnd p2 x 
        fork$ do x <- getSnd p2
                 putSnd p1 x
        putFst p1 33
        getSnd p1
