
-- | Various kinds of IStructures or arrays of IVars.

{-# LANGUAGE TemplateHaskell, CPP, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module ArrayTests where

import Test.Framework.Providers.HUnit 
import Test.Framework (Test, defaultMain, testGroup)
-- [2013.09.26] Temporarily disabling template haskell due to GHC bug discussed here:
--   https://github.com/rrnewton/haskell-lockfree/issues/10
import Test.Framework.TH (testGroupGenerator)
import Test.HUnit (Assertion, assertEqual, assertBool, Counts(..))
import qualified Test.HUnit as HU
import TestHelpers (defaultMainSeqTests)

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

  -- TODO: Remove most of this!  This file should not tests LVars other than IVars:

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

import Debug.Trace
import TestHelpers as T

--------------------------------------------------------------------------------

runTests :: IO ()
runTests = defaultMainSeqTests [tests]

-- SADLY, this use of template-Haskell, together with the atomic-primops dependency,
-- triggers a GHC linking bug:
tests :: Test
tests = $(testGroupGenerator)

--------------------------------------------------------------------------------
-- NatArrays
--------------------------------------------------------------------------------

case_v9a :: Assertion
case_v9a = assertEqual "basic NatArray" 4 =<< v9a
v9a :: IO Word8
v9a = runParIO$ do
  arr <- NA.newNatArray 10
  NA.put arr 5 (4::Word8)
  NA.get arr 5


-- #ifdef NO_DANGLING_THREADS
-- case_i9b :: Assertion
-- case_i9b = exceptionOrTimeOut 0.3 [] i9b
-- -- | A test to make sure that we get an error when we should.
-- i9b :: IO Word8
-- i9b = runParIO$ do
--   arr:: NA.NatArray s Word8 <- NA.newNatArray 10 
--   fork $ do NA.get arr 5
--             logDbgLn "Unblocked!  Shouldn't see this."
--             return ()
--   return 9
-- #endif

case_i9c :: Assertion
case_i9c = exceptionOrTimeOut 0.3 ["thread blocked indefinitely"] i9c
i9c :: IO Word8
i9c = runParIO$ do
  arr:: NA.NatArray s Word8 <- NA.newNatArray 10 
  fork $ do NA.get arr 5
            logDbgLn 1 "Unblocked!  Shouldn't see this."
            NA.put arr 6 99
  NA.get arr 6 

case_v9d :: Assertion
case_v9d = assertEqual "NatArray blocking/unblocking" 99 =<< v9d
v9d :: IO Word8
v9d = runParIO$ do
  arr:: NA.NatArray s Word8 <- NA.newNatArray 10 
  fork $ do NA.get arr 5
            logDbgLn 1 "Unblocked! Good."
            NA.put arr 6 99
  logDbgLn 1 "After fork."
  NA.put arr 5 5
  NA.get arr 6 

in9e :: Int
in9e = case numElems of
        Just x -> x
        -- 100000  -- This was where lots of problems happen.        
        Nothing -> 10000 -- Wait... still plenty of problems at this size.

out9e :: Word64
out9e = fromIntegral$ in9e * (in9e + 1) `quot` 2 -- 5000050000

-- | Fill in all elements of a NatArray, and then sum them.
v9e :: IO Word64
v9e = runParIO$ do
  let size = in9e
  arr <- NA.newNatArray size
  fork $
    forM_ [0..size-1] $ \ix ->
      NA.put arr ix (fromIntegral ix + 1) -- Can't put 0
  logDbgLn 1 $ "v9e: After fork.  Filling array of size "++show size
  let loop !acc ix | ix == size = return acc
                   | otherwise  = do v <- NA.get arr ix
                                     loop (acc+v) (ix+1)
  loop 0 0
-- NOTE: this test takes about 0.03 seconds (with input size ?????)
-- It is not faster with two threads, alas... but it is higher variance!

-- WARNING: I'm seeing some livelocks here that depend on the number of threads
-- (e.g. at -N4 but not -N2).  When deadlocked on -N4 it burns 250% cpu.
-- 
-- [2013.08.05] Update... it can pass 100 iterations at -N4 BY ITSELF,
-- but fails much more rapidly when run together with other 'v9'
-- tests.
case_v9e_NatArr :: Assertion
case_v9e_NatArr = assertEqual "Scale up a bit" out9e =<< v9e


-- Uh oh, this is blocking indefinitely sometimes...
-- BUT, only when I run the whole test suite.. via cabal install --enable-tests
case_i9h :: Assertion
case_i9h = exceptionOrTimeOut 0.3 ["Attempt to put zero"] i9i
i9i :: IO Word
i9i = runParIO$ do
  arr <- NA.newNatArray 1
  NA.put arr 0 0
  NA.get arr 0

--------------------------------------------------------------------------------
-- Array of IVar
--------------------------------------------------------------------------------

-- | Here's the same test  as v9e with an actual array of IVars.
--   This one is reliable, but takes about 0.20-0.30 seconds.
case_v9f_ivarArr :: Assertion
-- [2013.08.05] RRN: Actually I'm seeing the same non-deterministic
-- thread-blocked-indefinitely problem here.
-- [2013.12.13] It can even happen at NUMELEMS=1000 (with debug messages slowing it)
--              Could this possibly be a GHC bug?
-- [2013.12.13] Runaway duplication of callbacks is ALSO possible on this test.
--              Bafflingly that happens on DEBUG=2 but not 5.
case_v9f_ivarArr = assertEqual "Array of ivars, compare effficiency:" out9e =<< v9f
v9f :: IO Word64
v9f = runParIO$ do
  let size = in9e
      news = V.replicate size IV.new
  arr <- V.sequence news
  fork (do logDbgLn 1 " [v9f] Beginning putter loop.."
           forM_ [0..size-1] $ \ix ->
             IV.put_ (arr V.! ix) (fromIntegral ix + 1))
  logDbgLn 1 " [v9f] After fork."
  let loop !acc ix | ix == size = return acc
                   | otherwise  = do v <- IV.get (arr V.! ix)
                                     when (ix `mod` 1000 == 0) $
--                                       trace ("   [v9f] get completed at: "++show ix) $ return ()
                                       logDbgLn 2 $ "   [v9f] get completed at: "++show ix++" -> "++show v
                                     loop (acc+v) (ix+1)
  loop 0 0



--------------------------------------------------------------------------------
-- IStructure
--------------------------------------------------------------------------------

-- | One more time with a full IStructure.
case_v9g_istruct :: Assertion
case_v9g_istruct = assertEqual "IStructure, compare effficiency:" out9e =<< v9g
v9g :: IO Word64
v9g = runParIO$ do
  let size = in9e
  arr <- ISt.newIStructure size      
  fork $
    forM_ [0..size-1] $ \ix ->
      ISt.put_ arr ix (fromIntegral ix + 1)
  logDbgLn 1 "After fork."
  let loop !acc ix | ix == size = return acc
                   | otherwise  = do v <- ISt.get arr ix
                                     loop (acc+v) (ix+1)
  loop 0 0


case_show04 :: Assertion
case_show04 = assertEqual "show for IStructure" "{IStructure: Just 33, Just 44}" show04
show04 :: String
show04 = show$ runParThenFreeze $ do
  ist <- ISt.newIStructure 2
  ISt.put ist 0 (33::Int)
  ISt.put ist 1 (44::Int)
  return ist

