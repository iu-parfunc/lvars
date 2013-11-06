{-# LANGUAGE TemplateHaskell, CPP, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Tests for SNZI data structure.
module SkipListTests where

import Test.Framework.Providers.HUnit 
import Test.Framework (Test, defaultMain, testGroup)
-- [2013.09.26] Temporarily disabling template haskell due to GHC bug discussed here:
--   https://github.com/rrnewton/haskell-lockfree/issues/10
import Test.Framework.TH (testGroupGenerator)

import Test.HUnit (Assertion, assertEqual, assertBool, Counts(..))
import qualified Test.HUnit as HU
import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import GHC.Conc
import Data.List (isInfixOf, intersperse)
import qualified Data.Vector as V
import qualified Data.Set as S
import Data.IORef
import Data.Time.Clock
import System.Environment (getArgs)
import System.IO
import System.Exit
import System.Random

import Control.Exception (catch, evaluate, SomeException)

import Data.Traversable (traverse)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Word

import qualified Data.LVar.Generic as G
import qualified Data.LVar.NatArray as NA
import Data.LVar.PureSet as IS
import Data.LVar.PureMap as IM

import qualified Data.LVar.SLMap as SM
import qualified Data.LVar.SLSet as SS
import Data.LVar.Memo  as Memo

import qualified Data.LVar.IVar as IV
import qualified Data.LVar.IStructure as ISt
import qualified Data.LVar.Pair as IP

import Control.LVish
import Control.LVish.DeepFrz (DeepFrz(..), Frzn, Trvrsbl, runParThenFreeze, runParThenFreezeIO)
import qualified Control.LVish.Internal as I
import Control.LVish.SchedIdempotent (liftIO, dbgLvl, forkWithExceptions)
import qualified Control.LVish.SchedIdempotent as L

import qualified Data.Concurrent.SNZI as SNZI
import qualified Data.Concurrent.LinkedMap as LM
import qualified Data.Concurrent.SkipListMap as SLM

import TestHelpers as T

--------------------------------------------------------------------------------
-- TESTS FOR SKIPLIST
--------------------------------------------------------------------------------

lm1 :: IO (String)
lm1 = do
  lm <- LM.newLMap
  LM.NotFound tok <- LM.find lm 1
  LM.tryInsert tok "Hello"
  LM.NotFound tok <- LM.find lm 0
  LM.tryInsert tok " World"
  LM.Found s1 <- LM.find lm 1
  LM.Found s0 <- LM.find lm 0
  return $ s1 ++ s0
  
case_lm1 :: Assertion  
case_lm1 = lm1 >>= assertEqual "test sequential insertion for LinkedMap" "Hello World"

slm1 :: IO (String)
slm1 = do
  slm <- SLM.newSLMap 5
  SLM.putIfAbsent slm 0 $ return "Hello "
  SLM.putIfAbsent slm 1 $ return "World"
  Just s0 <- SLM.find slm 0
  Just s1 <- SLM.find slm 1
  return $ s0 ++ s1
  
case_slm1 :: Assertion  
case_slm1 = slm1 >>= assertEqual "test sequential insertion for SkipListMap" "Hello World"  

slm2 :: IO Bool
slm2 = do
  slm <- SLM.newSLMap 10
  mvars <- replicateM numCapabilities $ do
    mv <- newEmptyMVar
    forkWithExceptions forkIO "slm2 test thread" $ do
      rgen <- newIORef $ mkStdGen 0
      let flip = do
            g <- readIORef rgen
            let (b, g') = random g
            writeIORef rgen $! g'
            return b
      nTimes 10000 $ \n -> SLM.putIfAbsentToss slm n (return n) flip
      putMVar mv ()
    return mv  
  forM_ mvars takeMVar
  -- cs <- SLM.counts slm
  -- putStrLn $ show cs
  SLM.foldlWithKey (\b k v -> if k == v then return b else return False) True slm
--  Just n <- SLM.find slm (slm2Count/2)  -- test find function
--  return n
  
case_slm2 :: Assertion  
case_slm2 = slm2 >>= assertEqual "test concurrent insertion for SkipListMap" True


--------------------------------------------------------------------------------

tests :: Test
tests = $(testGroupGenerator)

runTests :: IO ()
runTests = defaultMain [tests]
