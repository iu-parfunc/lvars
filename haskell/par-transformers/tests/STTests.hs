{-# LANGUAGE TemplateHaskell #-}

module STTests (tests, runTests) where

import Control.LVish      as LV
import qualified Data.LVar.IVar as IV
import Control.Par.StateT as S

import Control.Monad
import Control.Monad.IO.Class
import qualified Control.Monad.Trans.State.Strict as S
import Control.Monad.ST        (ST)
import Control.Monad.ST.Unsafe (unsafeSTToIO)
import Control.Monad.Trans (lift)
import Control.Monad.State (get,put)

import Control.Concurrent (threadDelay)

import Data.STRef
import Data.Vector.Mutable as MV
import Data.Vector       (freeze)
import Debug.Trace

import Prelude hiding (read, length)
import System.IO.Unsafe (unsafePerformIO)

import GHC.Prim (RealWorld)

import qualified Control.Par.Class as PC

import Test.HUnit (Assertion, assert, assertEqual, assertBool, Counts(..))
import Test.Framework.Providers.HUnit
import Test.Framework -- (Test, defaultMain, testGroup)
import Test.Framework.TH (testGroupGenerator)

import qualified Control.Par.ST.Vec as V
import qualified Control.Par.ST.Vec2 as VV
import qualified Control.Par.ST as PST

import Control.Par.Class.Unsafe (ParThreadSafe(unsafeParIO))

--------------------------------------------------------------------------------

--- New Tests for Par/Vec

case_v_t0 :: Assertion
case_v_t0 = assertEqual "basic forkSTSplit usage"
            "fromList [5,0,0,0,0,120,0,0,0,0]" t0
            
t0 :: String            
t0 = LV.runPar $ V.runParVecT 10 p0

p0 :: V.ParVecT s1 Int (LV.Par e s0) String
p0 = do
  
  V.set 0
  
  V.forkSTSplit 5
    (do V.write 0 5)
    (do V.write 0 120)
    
  raw <- V.reify
  frozen <- PST.liftST$ freeze raw  
         
  return$ show frozen


case_v_t1 :: Assertion
case_v_t1 = assertEqual "testing transmute"
            "fromList [0]fromList [0]" t1
            
t1 :: String
t1 = LV.runPar $ V.runParVecT 1 p1

p1 :: V.ParVecT s1 Int (LV.Par e s0) String
p1 = do  
  V.set 0  
  PST.transmute (\v -> PST.STTup2 v v) 
    (do 
        (rawL, rawR) <- VV.reify
        frozenL <- PST.liftST$ freeze rawL
        frozenR <- PST.liftST$ freeze rawR
        return$ show frozenL ++ show frozenR)
  
case_v_t2 :: Assertion
case_v_t2 = assertEqual "testing transmute with effects"
                 "fromList [120,5] fromList [120,5]fromList [120,5]" t2
            
t2 :: String
t2 = LV.runPar $ V.runParVecT 2 p2

-- | FIXME: This is an example of what we should NOT be allowed to do.
--   Arbitrary transmute can't be allowed, it allows aliasing.
--   However, controlled zooming in and out will be allowed.
p2 :: V.ParVecT s1 Int (LV.Par e s0) String
p2 = do
  V.set 0
  str <- PST.transmute (\v -> PST.STTup2 v v)
    (do 
        VV.writeL 0 120
        VV.writeR 1 5
        (rawL,rawR) <- VV.reify
        frozenL <- PST.liftST$ freeze rawL
        frozenR <- PST.liftST$ freeze rawR
        return$ show frozenL ++ show frozenR)
           
  raw <- V.reify
  frozen <- PST.liftST$ freeze raw
  let result = show frozen ++ " " ++ str
  return result
{-
splitVec v = (PST.STTup2 (PST.VFlp l) (PST.VFlp r))
  where
    len = length v
    mid = len `quot` 2
    l = MV.slice 0 mid v
    r = MV.slice mid (len - mid) v
-}
--------------------------------------------------------------------------------
  
tests :: Test
tests = $(testGroupGenerator)

runTests :: IO ()
runTests = defaultMain [tests]

