{-# LANGUAGE TemplateHaskell, CPP, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

-- | Core tests for the LVish scheduler and basic futures/IVars.

module LVishAndIVar(tests,runTests, runParStress, lotsaRunPar) where

import Test.Tasty.HUnit 
import Test.Tasty (TestTree, defaultMain, testGroup)
-- [2013.09.26] Temporarily disabling template haskell due to GHC bug discussed here:
--   https://github.com/rrnewton/haskell-lockfree/issues/10
import Test.Tasty.TH (testGroupGenerator)

--import Test.HUnit (Assertion, assertEqual, assertBool)
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
import Debug.Trace
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
import           Control.LVish.Internal.SchedIdempotent (liftIO, dbgLvl, forkWithExceptions)
import qualified Control.LVish.Internal.SchedIdempotent as L

import           TestHelpers2 as T

--------------------------------------------------------------------------------

runTests :: IO ()
runTests = defaultMain tests

-- SADLY, this use of template-Haskell, together with the atomic-primops dependency,
-- triggers a GHC linking bug:
tests :: TestTree
tests = $(testGroupGenerator)

--------------------------------------------------------------------------------

-- | This stress test does nothing but run runPar again and again.
case_runParStress :: HU.Assertion
case_runParStress = runParStress
runParStress :: HU.Assertion
runParStress = stressTest T.stressTestReps 15 (return ()) (\()->True)

-- TEMP: another version that uses the simplest possible method to run lots of runPars.
-- Nothing else that could POSSIBLY get in the way.
-- 
-- [2014.01.16] Very odd!  I'm not able to get the crash here, but I am for the
-- runParsStress test...  Actually, I'm having trouble getting the crash with less
-- than 20 threads in the runtime for the old test too.  That is, the first of these
-- crashes quickly, and the second one won't crash for me:
--
--     STRESSTESTS=15000 ./LVishAndIVar.exe -t runParStress +RTS -N20
--     STRESSTESTS=15000 ./LVishAndIVar.exe -t runParStress +RTS -N15
-- 
-- Oddly, it seems to go from happening rarely at -N17 to often at -N18.
-- I think the problem with the simple test that uses "runParNonDet" is that we 
-- can't make the runtime use more capabilities than we fork par worker threads.
-- This could be a GHC runtime bug relating to thread migration?
case_lotsaRunPar :: Assertion
case_lotsaRunPar = lotsaRunPar
lotsaRunPar = loop iters
  where 
  iters = 5000
  threads = 15 -- numCapabilities 
  loop 0 = putStrLn ""
  loop i = do
     -- We need to do runParNonDet to make sure the compiler does the runPar each time.
     -- runParNonDet (return ()) -- Can't crash this one.
     runParDetailed (DbgCfg Nothing [] False) threads (return ())
      -- This version can start going RIDICULOUSLY slowly with -N20.  It will use <20% CPU while it does it.
      -- But it won't use much memory either... what is it doing?  With -N4 it goes light years faster, and with -N2
      -- faster yet.  Extra capabilities result in a crazy slowdown here.
      -- With the bad behavior on -N20, it will SOMETIMES complete 5000 iterations in ~3 seconds.  But sometimes
      -- it will grind to a snails pace after a few hundred iterations.  
      -- The missing time doesn't show up as system or CPU time...
      -- At -N15, where the # workers matches the # capabilities, it keeps up an ok pace...
      --   -qa doesn't seem to help the problem.
      --   -qm seems to EXACERBATE the problem, making it happen from the start and consistently. 
      --    (even then, it is fine with -N15, the mismatch is the problem)
      --   Playing around with -C, -qb -qg -qi doesn't seem to do anything.
     -- traceEventIO ("Finish iteration "++show (iters-i))
     -- For debugging I put in this traceEvent and ran with +RTS -N18 -qm -la
     putStr "."; hFlush stdout
     loop (i-1)

-- Disabling thread-variation due to below bug:

-- EEK!  Just got this [2013.06.27]:
-- 
-- unit-tests.exe: internal error: wakeup_gc_threads
--     (GHC version 7.6.3 for x86_64_unknown_linux)
--     Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug
-- Aborted (core dumped)

v0 :: Par (Ef P G F B I) s Int
v0 = do i <- IV.new; fork (return ()); IV.put i 4; IV.get i

case_v0 :: HU.Assertion
case_v0 = stressTest T.stressTestReps 3 v0 (== 4)
                            
case_v1a :: Assertion
case_v1a = stressTest T.stressTestReps 3 v1a (== (4::Int))

v1a :: Par (Ef P G F B I) s Int
v1a = do i<-IV.new; fork (IV.put i 4); IV.get i

case_v1b :: Assertion
case_v1b = do ls <- v1b
              case length ls of
                0 -> return () -- Ok, i guess debugging is off.
                1 -> return () 
                _ -> error $ "Wrong number of log messages: \n" ++ concat (intersperse "\n" ls)

-- | In this sequential case there should be no data-race, and thus no duplication of the callback.
v1b :: IO [String]
v1b = do let tag = "callback on ivar "
         (logs,_) <- runParLogged $ isDet $ do
                       i <- IV.new
                       IV.put i (3::Int)                       
                       IV.whenFull Nothing i (\x -> logDbgLn 1$ tag++show x)
                       IV.put i 3
                       IV.put i 3
                       return ()
         mapM_ putStrLn logs
         return (filter (isInfixOf tag) logs)

escape01 :: IV.IVar Frzn Int
escape01 = runParThenFreeze $ isDet $
           do v <- IV.new; IV.put v (3::Int); return v

-- | This is VERY BAD:
escape01B :: HasPut e => Par e Frzn String
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
i3f = runParQuasiDet$ isQD $ do
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
i3g = runParQuasiDet$ isQD $ do
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
lp01 = runParQuasiDet$ isQD $ do
  logDbgLn 2 " [lp01] Starting parForSimple loop..."
  x <- IV.new 
  parForSimple (0,10) $ \ ix -> do
    logDbgLn 2$ " [lp01]  iter "++show ix
    when (ix == 9)$ IV.put x "done"
  IV.get x

case_lp02 :: Assertion
case_lp02 = assertEqual "parForL test" "done" =<< lp02
lp02 = runParQuasiDet$ isQD$ do
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
lp03 = runParQuasiDet$ isQD $ do
  logDbgLn 2 " [lp03] Starting parForTree loop..."
  x <- IV.new 
  parForTree (0,10) $ \ ix -> do
    logDbgLn 2$ " [lp03]  iter "++show ix
    when (ix == 9)$ IV.put x "done"
  logDbgLn 2$ " [lp03] after loop..."
  IV.get x

case_lp04 :: Assertion
case_lp04 = assertEqual "parForTree test" "done" =<< lp04
lp04 = runParQuasiDet$ isQD $ do
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
-- v4 = runParNonDet $
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
-- i5a = runParNonDet (
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
--   runParNonDet $
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

-- i5c = runParNonDet $
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
-- i6a = runParNonDet (
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
--           33 =<< runParNonDet (
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
dftest0 = runParNonDet $ isND $ do
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
dftest1 = runParNonDet $ isND $ do
  iv1 <- IV.new
  iv2 <- IV.new
  IV.put_ iv1 iv2
  IV.put_ iv2 "hello"
  Just x <- IV.freezeIVar iv1
  IV.freezeIVar x

case_dftest3 = assertEqual "freeze simple ivar" (Just 3) =<< dftest3
dftest3 :: IO (Maybe Int)
dftest3 = runParNonDet $ isND $ do
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
-- dftest4a = runParNonDet dftest4_

------------------------------------------------------------------------------------------
-- Show instances
------------------------------------------------------------------------------------------

case_show01 :: Assertion
case_show01 = assertEqual "show for IVar" "Just 3" show01
show01 :: String
show01 = show$ runParThenFreeze $ isDet $ do v <- IV.new; IV.put v (3::Int); return v

