{-# LANGUAGE CPP, ConstraintKinds, DataKinds, RankNTypes, ScopedTypeVariables,
             TemplateHaskell, TypeFamilies #-}

module CancelTests (tests, runTests)
       where

import Control.LVish (isDet, isND, isQD, isReadOnly, liftReadOnly, logDbgLn,
                      runPar, runParLogged, runParNonDet)

import Control.LVish.CancelT as CT
import Control.LVish.Internal
import Control.Par.Class
import Control.Par.Class.Unsafe
import Control.Par.EffectSigs

import Control.Concurrent
import Data.List (isInfixOf)

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.TH (testGroupGenerator)
import Test.HUnit (Assertion, Counts (..), assert, assertBool, assertEqual)

import Debug.Trace

tests :: Test
tests = $(testGroupGenerator)

runTests :: IO ()
runTests = defaultMain [tests]

--------------------------------------------------------------------------------
-- Helpers:

appreciableDelay :: IO ()
appreciableDelay = threadDelay (100 * 1000)

dbg :: String -> CancelT Par e s ()
dbg = lift . logDbgLn 0

-- FIXME: Logging has some problems:
-- - It depends on env variable DEBUG.
-- - However, when DEBUG=0, `logDbgLn 0 msg` doesn't log anything. It has to be
--   at least 1.
-- - But when it's 1, it prints lots of internal log messages too.
-- - Also, LVish needs to be compiled with -DDEBUG_LVAR for logging.
-- - It also adds some prefix to log messages. I think those prefixes are
--   non-deterministic, becuase worker thread ID is part of the prefix. So we
--   can't directly compare log messages in tests.
--
-- Currently I'm not fixing anything. I'll write some tests for specifically
-- logging. For now we should search log messages in all collected messages in
-- tests:

-- | Assert that given log message is infix of at least one log message.
assertHasLog :: String -> [String] -> Assertion
assertHasLog log allLogs =
  assertBool ("Can't find the log message " ++ show log ++ " in logs: " ++ show allLogs) $
    any (log `isInfixOf`) allLogs

-- | Assert that given log message is not infix of any log messages.
assertDoesntHaveLog :: String -> [String] -> Assertion
assertDoesntHaveLog log allLogs =
  assertBool ("Found the log message " ++ show log ++ " in logs: " ++ show allLogs) $
    not $ any (log `isInfixOf`) allLogs


-- Making sure the logger is working with Par.
case_test0_Par :: IO ()
case_test0_Par = do
  let logMsg = "Testing the logger."
  (logs, _) <- runParLogged $ logDbgLn 0 logMsg
  assertHasLog logMsg logs

-- Make sure logging is working with CancelT.
case_test0_CancelT :: IO ()
case_test0_CancelT = do
  let logMsg = "Testing the logger."
  (logs, _) <- runParLogged $ isDet $ CT.runCancelT $ dbg logMsg
  assertHasLog logMsg logs

case_cancel01 :: IO ()
case_cancel01 = do
  (logs, _) <- cancel01
  assertHasLog "Begin test 01" logs
  assertDoesntHaveLog "Past cancelation point!" logs

cancel01 :: IO ([String], ())
cancel01 = runParLogged $ isDet $ CT.runCancelT $ do
  dbg "Begin test 01"
  cancelMe -- this is just returnToSched from LVarSched method of Par
           -- FIXME: seems like log state is not passed
  dbg "Past cancelation point!"


-- case_cancel02 :: IO ()
-- case_cancel02 = do
--   (lines, _) <- cancel02
--   assertEqual "Read wrong number of outputs" 4 (length lines)
--   assertEqual "Read an error message" False (any (isInfixOf "!!") lines)

-- | This should always cancel the child before printing "!!".
cancel02 :: IO ([String], ())
cancel02 =
  runParLogged $ isDet $ runCancelT comp
 where
   comp :: forall p e s a .
           (GetG e ~ G, HasPut e,
            GetG (SetReadOnly e) ~ GetG e) =>
           CancelT Par e s ()
   comp = do
     dbg "[parent] Begin test 02"
     iv <- new
     let p1 :: CancelT Par (SetReadOnly e) s ()
         p1 = do
           dbg "[child] Running on child thread... block so parent can run"
           -- lift $ Control.LVish.yield -- Not working!
           -- Do we need this? ^ Next line will force rescheduling.
           get iv -- This forces the parent to get scheduled.
           dbg "[child] Woke up, now wait so we will be cancelled..."
           internalLiftIO appreciableDelay
           pollForCancel
           dbg "!! [child] thread got past delay!"

         -- p2 :: CancelT p e s ()
         -- p2 = forkCancelable undefined

  --        p3 :: CancelT m1 (CT.ThreadId, CFut m1 ())
  --        p3 = undefined -- This is ok.
  --        -- Then here we fail, with a strange "Couldn't match type 'NP with 'P":
         -- p3 = forkCancelable undefined

{-
   -- --      p2 :: forall s . CancelT (Par (Ef NP G NF NB NI) s) b
   --       p2 :: m2 (CT.ThreadId, CFut m1 ())
   --       p2 = undefined -- forkCancelable p


   --      p2 = forkCancelable p
   --  (tid,cfut) <- liftReadOnly2 $ forkCancelable p
-}
     let tid = undefined
     put iv ()
     cancel tid
     dbg "[parent] Issued cancel, now exiting."

--------------------------------------------------------------------------------
-- Tests:

{-
-- case_cancel01 =

-- type MyM a = CancelT (Par e s)

-- | This deadlocks, because the last computation was canceled!

-- TODO: Need to catch the deadlock and put this in a timeout:


-- | A variant of liftReadOnly which works on a particular transformer
-- stack... creating the GENERAL version of this has proven difficult.
liftReadOnly2 :: CancelT (Par (Ef NP g NF NB NI) s) a -> CancelT (Par (Ef p g f b i) s) a
liftReadOnly2 = error "FINISHME"



isDet2 :: (e ~ (Ef P G NF B NI)) => CancelT (Par e s) a -> CancelT (Par e s) a
isDet2 x = x


isRO :: (e ~ (Ef NP G NF NB NI)) => CancelT (Par e s) a -> CancelT (Par e s) a
isRO x = x

#if 1
cancel02B :: IO ()
cancel02B = runParPolyIO$ runCancelT $ do
  dbg$ "Begin test 02B"
--  _ <- isRO$ forkTemp $ isRO$ do
  _ <- isRO$ id $ isRO$ do
      dbg$ "(1) Running on child thread..."
      cancelMe
      dbg$ "(2) Running on child thread..."
  dbg$ "Waiting on main thread..."
  io$ appreciableDelay
  dbg$ "Now exiting on main thread."
  return ()
#endif

-- | DEBUGGING -- this really should look like an identity function:
forkTemp :: ( PC.ParIVar m, PC.LVarSched m
             -- ReadOnlyM m,
             -- e ~ GetEffects m,
             -- NoPut e,
            -- , PC.FutContents m CFutFate
--            , PC.FutContents m a -- This line alone screws it up:
                                 --   Couldn't match type 'NP with 'P
            ) =>
            (CancelT m a -> CancelT m a)
forkTemp = error "FINISHME"

ro1 :: CancelT (Par (Ef NP G NF NB NI) s) ()
ro1 = undefined

ro2 :: CancelT (Par (Ef NP G NF NB NI) s) ()
-- ro2 = id ro1
ro2 = forkTemp ro1 -- Activate WEIRD bug.


main = cancel02B

------------------------------------------------------------

cancel03 :: IO ()
cancel03 = runParNonDet$ runCancelT $ do
  dbg$ "Begin test 03"
  tid <- forkCancelableND $ do
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

-- case_and1 :: Assertion
-- case_and1 = assertEqual "" False $ runPar $ runCancelT $ do
--               v <- PC.new
--               CT.asyncAndCPS (return True) (return False) (PC.put v)
--               PC.get v

-- case_and2 :: Assertion
-- case_and2 = assertEqual "" False $ runPar $ runCancelT $ do
--               v <- PC.new
--               CT.asyncAnd (return False) (return False) (PC.put v)
--               PC.get v

-- case_and3 :: Assertion
-- case_and3 = assertEqual "" True $ runPar $ runCancelT $ do
--               v <- PC.new
--               CT.asyncAnd (return True) (return True) (PC.put v)
--               PC.get v

-- case_and4 :: Assertion
-- case_and4 = assertEqual "" False $ runPar $ runCancelT $ do
--               v <- PC.new
--               CT.asyncAnd (return False) (return True) (PC.put v)
--               PC.get v


-- case_andTreeF :: Assertion
-- case_andTreeF = assertEqual "" False $ runPar $ runCancelT $ andTreeF 16

-- case_andTreeT :: Assertion
-- case_andTreeT = assertEqual "" True $ runPar $ runCancelT $ andTreeT 16

-- -- | Takes a depth N and does 2^N operations in a binary tree
-- andTreeF :: Int -> CancelT (Par e s) Bool
-- andTreeF 0 = return False
-- andTreeF depth = do
--   v <- PC.new
--   CT.asyncAnd (andTreeF (depth-1)) (andTreeF (depth-1)) (PC.put v)
--   PC.get v

-- andTreeT :: Int -> CancelT (Par e s) Bool
-- andTreeT 0 = return True
-- andTreeT depth = do
--   v <- PC.new
--   CT.asyncAnd (andTreeT (depth-1)) (andTreeT (depth-1)) (PC.put v)
--   PC.get v



-- TODO: tree of ANDs with cancellation..
{-
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
-}

-}
