{-# LANGUAGE TemplateHaskell, CPP, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Core tests for the LVish scheduler and basic futures/IVars.

module LVishAndIVar(tests, runTests) where

import Test.Framework.Providers.HUnit 
import Test.Framework (Test, defaultMain, testGroup)
-- [2013.09.26] Temporarily disabling template haskell due to GHC bug discussed here:
--   https://github.com/rrnewton/haskell-lockfree/issues/10
import Test.Framework.TH (testGroupGenerator)

import Test.HUnit (Assertion, assertEqual, assertBool)
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

import qualified Data.LVar.IVar as IV

import           Control.LVish
import           Control.LVish.DeepFrz (DeepFrz(..), Frzn, Trvrsbl, runParThenFreeze, runParThenFreezeIO)
import qualified Control.LVish.Internal as I
import           Control.LVish.SchedIdempotent (liftIO, dbgLvl, forkWithExceptions)
import qualified Control.LVish.SchedIdempotent as L

import           TestHelpers as T

--------------------------------------------------------------------------------

runTests :: IO ()
runTests = defaultMain [tests]

-- SADLY, this use of template-Haskell, together with the atomic-primops dependency,
-- triggers a GHC linking bug:
tests :: Test
tests = $(testGroupGenerator)

--------------------------------------------------------------------------------

-- Disabling thread-variation due to below bug:

-- EEK!  Just got this [2013.06.27]:
-- 
-- unit-tests.exe: internal error: wakeup_gc_threads
--     (GHC version 7.6.3 for x86_64_unknown_linux)
--     Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug
-- Aborted (core dumped)

case_v0 :: HU.Assertion
case_v0 = do res <- v0
             HU.assertEqual "useless fork" (4::Int) res
v0 = runParIO $ do i <- IV.new; fork (return ()); IV.put i 4; IV.get i


case_v1a :: Assertion
case_v1a = assertEqual "fork put" (4::Int) =<< v1a
v1a :: IO Int
v1a = runParIO $ do i<-IV.new; fork (IV.put i 4); IV.get i

case_v1b :: Assertion
case_v1b = do ls <- v1b
              case length ls of
                0 -> return () -- Ok, i guess debugging is off.
                1 -> return () 
                _ -> error $ "Wrong number of log messages: \n" ++ concat (intersperse "\n" ls)

-- | In this sequential case there should be no data-race, and thus no duplication of the callback.
v1b :: IO [String]
v1b = do let tag = "callback on ivar "
         (logs,_) <- runParLogged $ do
                       i <- IV.new
                       IV.put i (3::Int)                       
                       IV.whenFull Nothing i (\x -> logDbgLn 1$ tag++show x)
                       IV.put i 3
                       IV.put i 3
                       return ()
         mapM_ putStrLn logs
         return (filter (isInfixOf tag) logs)

escape01 :: IV.IVar Frzn Int
escape01 = runParThenFreeze $ do v <- IV.new; IV.put v (3::Int); return v

-- | This is VERY BAD:
escape01B :: Par d Frzn String
escape01B = 
            do IV.put escape01 (4::Int)
               return "uh oh"

-- | [2013.10.06] Fixed this by requiring a SPECIFIC type, NonFrzn.
-- major_bug :: String
-- major_bug = runParThenFreeze escape01B
               
-- | Simple callback test.
-- case_v3a :: Assertion
-- case_v3a = v3a >>= assertEqual "simple callback test"
--           (S.fromList [10,20,30,40,50,60,70,80,90,100] :: S.Set Int)


-- RRN: Currently we have a policy where leaving the seen with running threads is
-- disallowed, but blocked ones are tolerated.
case_i3f :: Assertion
case_i3f = exceptionOrTimeOut 0.3 ["test switched off"] i3f
i3f :: IO ()
#ifdef NO_DANGLING_THREADS
-- | A test to make sure that we get an error when we block on an unavailable ivar.
i3f = runParIO$ do
  iv <- IV.new
  fork $ do IV.get iv
            logDbgLn 1 "Unblocked!  Shouldn't see this."
            return ()
  return ()
#else 
i3f = error "test switched off"
#endif

case_i3g :: Assertion
case_i3g = exceptionOrTimeOut 0.3 [] i3g
-- | A still-running worker thread should NOT be allowed, because it may do a put that causes an exception.
i3g :: IO Word8
i3g = runParIO$ do
  iv <- IV.new
  fork $ do let loop !ls = loop [1 .. length ls]
            loop [1..10]
  return 9

--------------------------------------------------------------------------------
-- Higher level derived ops
--------------------------------------------------------------------------------  



--------------------------------------------------------------------------------
-- Looping constructs
--------------------------------------------------------------------------------

case_lp01 :: Assertion
case_lp01 = assertEqual "parForSimple test" "done" =<< lp01
lp01 = runParIO$ do
  logDbgLn 2 " [lp01] Starting parForSimple loop..."
  x <- IV.new 
  parForSimple (0,10) $ \ ix -> do
    logDbgLn 2$ " [lp01]  iter "++show ix
    when (ix == 9)$ IV.put x "done"
  IV.get x

case_lp02 :: Assertion
case_lp02 = assertEqual "parForL test" "done" =<< lp02
lp02 = runParIO$ do
  logDbgLn 2 " [lp02] Starting parForL loop..."
  x <- IV.new 
  parForL (0,10) $ \ ix -> do
    logDbgLn 2$ " [lp02]  iter "++show ix
    when (ix == 9)$ IV.put x "done"
  logDbgLn 2$ " [lp02] after loop..."
  IV.get x

-- [2013.08.05] RRN: I'm seeing this hang sometimes.  It live-locks
-- burning CPU.  (But only 170% CPU with -N4.)  Hmm, I can't get it to
-- freeze running BY ITSELF, however.  In fact I can't get the problem
-- while running just the "lp" tests.  I can get the problem running
-- just 'v' tests and even just 'v9' tests.
case_lp03 :: Assertion
case_lp03 = assertEqual "parForTree test" "done" =<< lp03
lp03 = runParIO$ do
  logDbgLn 2 " [lp03] Starting parForTree loop..."
  x <- IV.new 
  parForTree (0,10) $ \ ix -> do
    logDbgLn 2$ " [lp03]  iter "++show ix
    when (ix == 9)$ IV.put x "done"
  logDbgLn 2$ " [lp03] after loop..."
  IV.get x

case_lp04 :: Assertion
case_lp04 = assertEqual "parForTree test" "done" =<< lp04
lp04 = runParIO$ do
  logDbgLn 2 " [lp04] Starting parForTiled loop..."
  x <- IV.new 
  parForTiled Nothing 16 (0,10) $ \ ix -> do
    logDbgLn 2$ " [lp04]  iter "++show ix
    when (ix == 9)$ IV.put x "done"
  logDbgLn 2$ " [lp04] after loop..."
  IV.get x

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
-- Freeze-related tests:
--------------------------------------------------------------------------------

case_dftest0 = assertEqual "manual freeze, outer layer" "hello" =<< dftest0

dftest0 :: IO String
dftest0 = runParIO $ do
  iv1 <- IV.new
  iv2 <- IV.new
  IV.put_ iv1 iv2
  IV.put_ iv2 "hello"
  m <- IV.freezeIVar iv1
  case m of
    Just i -> IV.get i

case_dftest1 = assertEqual "deefreeze double ivar" (Just "hello") =<< dftest1

-- | Should return (Just (Just "hello"))
dftest1 :: IO (Maybe String)
dftest1 = runParIO $ do
  iv1 <- IV.new
  iv2 <- IV.new
  IV.put_ iv1 iv2
  IV.put_ iv2 "hello"
  Just x <- IV.freezeIVar iv1
  IV.freezeIVar x

case_dftest3 = assertEqual "freeze simple ivar" (Just 3) =<< dftest3
dftest3 :: IO (Maybe Int)
dftest3 = runParIO $ do
  iv1 <- IV.new
  IV.put_ iv1 (3::Int)
  IV.freezeIVar iv1 


--FIXME:

-- -- | Polymorphic version of previous.  DeepFrz is more flexible than regular
-- -- freeze, because we can pick multiple return types for the same code.  But we must
-- -- be very careful with this kind of thing due to the 's' type variables.
-- dftest4_ :: DeepFrz (IV.IVar s1 Int) =>
--             Par QuasiDet s1 b
-- dftest4_ = do
--   iv1 <- newBottom 
--   IV.put_ iv1 (3::Int)
--   res <- IV.freezeIVar iv1 
--   return res

-- case_dftest4a = assertEqual "freeze polymorphic 1" (Just 3) =<< dftest4a
-- dftest4a :: IO (Maybe Int)
-- dftest4a = runParIO dftest4_

------------------------------------------------------------------------------------------
-- Show instances
------------------------------------------------------------------------------------------

case_show01 :: Assertion
case_show01 = assertEqual "show for IVar" "Just 3" show01
show01 :: String
show01 = show$ runParThenFreeze $ do v <- IV.new; IV.put v (3::Int); return v

