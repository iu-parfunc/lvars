{-# LANGUAGE TemplateHaskell, DataKinds, TypeFamilies, ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

module CancelTests -- (tests, runTests)
       where

import Control.LVish (logDbgLn, runParLogged, runParNonDet, runPar, Par,
                      isDet, isQD, isND, isReadOnly, liftReadOnly)
import Control.LVish.CancelT as CT
import qualified Control.Par.Class as PC
import Control.Par.EffectSigs
import Control.Par.Class.Unsafe (ParMonad(internalLiftIO))
-- import Control.LVish.Unsafe ()
import Data.LVar.IVar as IV

import Control.Concurrent
import Control.Monad.Trans

import Data.List
import Test.HUnit (Assertion, assert, assertEqual, assertBool, Counts(..))
import Test.Framework.Providers.HUnit
import Test.Framework -- (Test, defaultMain, testGroup)
import Test.Framework.TH (testGroupGenerator)

--------------------------------------------------------------------------------
-- Helpers:

-- Invisible, untracked, unsafe IO:
io :: ParMonad m => IO a -> m a
io = internalLiftIO

appreciableDelay :: IO ()
appreciableDelay = threadDelay (100 * 1000)

-- FIXME: Need to replace this with something that will work when NOT in debug mode.
dbg :: String -> CancelT (Par e s) ()
dbg = lift . logDbgLn (-1)

--------------------------------------------------------------------------------
-- Tests:

-- case_cancel01 =

-- type MyM a = CancelT (Par e s)

-- | This deadlocks, because the last computation was canceled!
cancel01 :: IO ([String],())
cancel01 = runParLogged$ isDet $ runCancelT $ do
  dbg$ "Begin test 01"
  cancelMe
  dbg$ "Past cancelation point!"
  return ()

-- TODO: Need to catch the deadlock and put this in a timeout:
-- case_cancel01 :: IO ()
-- case_cancel01 = assertEqual "" ["Begin test 01"] =<< fmap fst cancel01 

-- | This should always cancel the child before printing "!!".
cancel02 :: forall m1 m2 s . 
            IO ([String],())
cancel02 = runParLogged$ isDet $ runCancelT $ do
  dbg$ "[parent] Begin test 02"
  iv  <- lift new
  let 
--      p :: forall s . CancelT (Par (Ef NP G NF NB NI) s) ()
      p :: (m1 ~ Par (Ef NP G NF NB NI) s,
            PC.ParIVar m1, PC.LVarSched m1, ReadOnlyM m1,  -- Sanity check.
            -- PC.FutContents m1 CFutFate,  -- ERROR HERE.  Why?
            -- PC.FutContents m1 (), -- ERROR HERE.  Why?
            m2 ~ CancelT m1)
        => m2 ()
      p = do dbg$ "[child] Running on child thread... block so parent can run"
             -- lift $ Control.LVish.yield -- Not working!
  --           lift$ get iv -- This forces the parent to get scheduled.
             dbg$ "[child] Woke up, now wait so we will be cancelled..."
             io$ appreciableDelay
             pollForCancel
             dbg$ "!! [child] thread got past delay!"

      -- p2 :: (m1 ~ Par (Ef NP G NF NB NI) s,
      --        m2 ~ CancelT m1)
      --    => m2 (CT.ThreadId, CFut m1 ())

      p2 :: (PC.ParIVar m, PC.LVarSched m, ReadOnlyM m, 
             PC.FutContents m CFutFate, PC.FutContents m a) => 
            CancelT m (CT.ThreadId, CFut m a)
      p2 = forkCancelable undefined

-- --      p2 :: forall s . CancelT (Par (Ef NP G NF NB NI) s) b
--       p2 :: m2 (CT.ThreadId, CFut m1 ())
--       p2 = undefined -- forkCancelable p

--      p2 = forkCancelable p
--  (tid,cfut) <- liftReadOnly2 $ forkCancelable p
  let tid = undefined
  lift$ put iv ()
  cancel tid
  dbg$ "[parent] Issued cancel, now exiting."
  return ()

-- | A variant of liftReadOnly which works on a particular transformer
-- stack... creating the GENERAL version of this has proven difficult.
liftReadOnly2 :: CancelT (Par (Ef NP g NF NB NI) s) a -> CancelT (Par (Ef p g f b i) s) a
liftReadOnly2 = error "FINISHME"

{-

case_cancel02 :: IO ()
case_cancel02 = do (lines,_) <- cancel02
--                   assertEqual "" 4 (length lines)
                   assertEqual "" False (or$ map (isInfixOf "!!") lines)

cancel03 :: IO ()
cancel03 = runParNonDet$ runCancelT $ do
  dbg$ "Begin test 03"
  tid <- forkCancelable $ do
      dbg$ "(1) Running on child thread..."
      cancelMe
      dbg$ "(2) Running on child thread..."

  dbg$ "Waiting on main thread..."
  io$ appreciableDelay
  dbg$ "Now exiting on main thread."
  return ()

--------------------------------------------------------------------------------
-- -- BOOLEAN TESTS:
-- --------------------------------------------------------------------------------
  
case_and1 :: Assertion
case_and1 = assertEqual "" False $ runPar $ runCancelT $ do
              v <- PC.new
              CT.asyncAnd (return True) (return False) (PC.put v)
              PC.get v

case_and2 :: Assertion
case_and2 = assertEqual "" False $ runPar $ runCancelT $ do
              v <- PC.new
              CT.asyncAnd (return False) (return False) (PC.put v)
              PC.get v

case_and3 :: Assertion
case_and3 = assertEqual "" True $ runPar $ runCancelT $ do
              v <- PC.new
              CT.asyncAnd (return True) (return True) (PC.put v)
              PC.get v                        

case_and4 :: Assertion
case_and4 = assertEqual "" False $ runPar $ runCancelT $ do
              v <- PC.new
              CT.asyncAnd (return False) (return True) (PC.put v)
              PC.get v


case_andTreeF :: Assertion
case_andTreeF = assertEqual "" False $ runPar $ runCancelT $ andTreeF 16

case_andTreeT :: Assertion
case_andTreeT = assertEqual "" True $ runPar $ runCancelT $ andTreeT 16

-- | Takes a depth N and does 2^N operations in a binary tree
andTreeF :: Int -> CancelT (Par e s) Bool
andTreeF 0 = return False
andTreeF depth = do
  v <- PC.new
  CT.asyncAnd (andTreeF (depth-1)) (andTreeF (depth-1)) (PC.put v)
  PC.get v

andTreeT :: Int -> CancelT (Par e s) Bool
andTreeT 0 = return True
andTreeT depth = do
  v <- PC.new
  CT.asyncAnd (andTreeT (depth-1)) (andTreeT (depth-1)) (PC.put v)
  PC.get v



-- TODO: tree of ANDs with cancellation..

-- case_or1 :: Assertion
-- case_or1 = assertEqual "" True $ runPar $ do
--               v <- IV.new
--               asyncOr Nothing (return True) (return False) (IV.put v)
--               IV.get v

-- case_or2 :: Assertion
-- case_or2 = assertEqual "" False $ runPar $ do
--               v <- IV.new
--               asyncOr Nothing (return False) (return False) (IV.put v)
--               IV.get v

-- case_or3 :: Assertion
-- case_or3 = assertEqual "" True $ runPar $ do
--               v <- IV.new
--               asyncOr Nothing (return True) (return True) (IV.put v)
--               IV.get v                        

-- case_or4 :: Assertion
-- case_or4 = assertEqual "" True $ runPar $ do
--               v <- IV.new
--               asyncOr Nothing (return False) (return True) (IV.put v)
--               IV.get v

-- case_andMap01 :: Assertion
-- case_andMap01 = assertEqual "" False $ runPar $
--                  andMap Nothing (return . even) [1..200::Int]

-- case_orMap01 :: Assertion
-- case_orMap01 = assertEqual "" True $ runPar $
--                 orMap Nothing (return . even) [1..200::Int]

-- -- TODO: add ones with explicit timing controls (sleep).

-- --------------------------------------------------------------------------------

tests :: Test
tests = $(testGroupGenerator)

-}

runTests :: IO ()
runTests = defaultMain [] -- [tests]

