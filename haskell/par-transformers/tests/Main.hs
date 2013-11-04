{-# LANGUAGE TemplateHaskell #-}

module Main where

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
import Prelude hiding (read, length)
import System.IO.Unsafe (unsafePerformIO)

import GHC.Prim (RealWorld)

import qualified Control.Par.Class as PC

import Test.HUnit (Assertion, assert, assertEqual, assertBool, Counts(..))
import Test.Framework.Providers.HUnit
import Test.Framework -- (Test, defaultMain, testGroup)
import Test.Framework.TH (defaultMainGenerator)

import qualified Control.Par.ST.Vec as V
import qualified Control.Par.ST as PST

import Control.Par.Class.Unsafe (ParThreadSafe(unsafeParIO))

--- New Tests for Par/Vec

case_v_t0 :: Assertion
case_v_t0 = assertEqual "basic forkSTSplit usage"
            "fromList [5,0,0,0,0,120,0,0,0,0]" t0
            
t0 :: String            
t0 = LV.runPar $ V.runParVecT 10 p0

p0 :: V.ParVecT s1 Int (LV.Par d s0) String
p0 = do
  
  V.set 0
  
  V.forkSTSplit 5
    (do V.write 0 5)
    (do V.write 0 120)
    
  raw <- V.reify
  frozen <- PST.liftST$ freeze raw  
         
  return$ show frozen

main = $(defaultMainGenerator)


