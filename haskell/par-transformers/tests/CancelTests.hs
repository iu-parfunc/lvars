{-# LANGUAGE CPP, ConstraintKinds, DataKinds, RankNTypes, ScopedTypeVariables,
             TemplateHaskell, TypeFamilies #-}

module CancelTests (tests, runTests)
       where

import Control.LVish (isDet, isND, isQD, isReadOnly, liftReadOnly, logDbgLn,
                      runPar, runParNonDet, runParDetailed)
import Control.LVish (DbgCfg(..), OutDest(..))
import Control.LVish.CancelT
import Control.LVish.Internal
import Control.Par.Class
import Control.Par.Class.Unsafe
import Control.Par.EffectSigs

import Control.Concurrent
import Data.List (isInfixOf)

import System.IO (stderr)

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


-- | A runner for testing which both grabs logs AND echos them to the screen.
runDbg :: (forall s . Par e s a) -> IO ([String], Either String a)
runDbg comp = do
  numCap <- getNumCapabilities
  (logs,ans) <- runParDetailed
                   DbgCfg { dbgRange = (Just (0,1))
                          , dbgDests = [OutputEvents, OutputInMemory, OutputTo stderr]
                          , dbgScheduling = False }
                   numCap comp
  case ans of
    Left err  -> return $! (logs, Left (show err))
    Right x   -> return $! (logs, Right x)

-- Making sure the logger is working with Par.
case_LogTest1 :: IO ()
case_LogTest1 = do
  let logMsg = "Testing the logger."
  (logs, _) <- runDbg $ logDbgLn 0 logMsg
  assertHasLog logMsg logs

-- Make sure logging is working with CancelT.
case_LogTest2 :: IO ()
case_LogTest2 = do
  let logMsg = "Testing the logger."
  (logs, _) <- runDbg $ isDet $ runCancelT $ dbg logMsg
  assertHasLog logMsg logs

-- Make sure `cancelMe` is not crashing anything. (no logging here)
case_cancelMe_NoLog :: IO ()
case_cancelMe_NoLog = runParNonDet $ runCancelT cancelMe

case_cancel01_CancelMe :: IO ()
case_cancel01_CancelMe = do
  logs <- cancel01
  assertHasLog "Begin test 01" logs
  assertDoesntHaveLog "Past cancelation point!" logs

-- FIXME: This seems to be a horrible behavior of test-framework: Exceptions
-- are not shown! At one point this test was failing with this:
--
-- (from GHCi)
--
-- -- > cancel01
-- -- ([],*** Exception: Control/LVish/CancelT.hs:56:10-49: No instance nor default method for class operation pbind
--
-- But test output was just "Failed".

cancel01 :: IO [String]
cancel01 = do
  (logs,Left err) <- runDbg $ isDet $ runCancelT $ do
       dbg "Begin test 01: about to cancelMe on main thread"
       cancelMe -- this is just returnToSched from LVarSched method of Par
                -- FIXME: seems like log state is not passed
       dbg "Past cancelation point!"
  assertBool "cancel01: correct error" $ "cancelMe: cannot be used" `isInfixOf` err
  return logs

case_cancel02 :: IO ()
case_cancel02 = do
  (lines, _) <- cancel02
  -- Can't compare number of logs, internal logs are interleaved with our logs
  -- assertEqual "Read wrong number of outputs" 4 (length lines)
  assertEqual "Read an error message" False (any (isInfixOf "!!") lines)

-- | This should always cancel the child before printing "!!".
cancel02 :: IO ([String], Either String ())
cancel02 =
  runDbg $ isDet $ runCancelT comp
 where
   comp :: forall p e s a .
           (HasGet e, HasPut e, HasGet (SetReadOnly e), HasPut (SetP 'P e)) =>
           CancelT Par e s ()
   comp = do
     dbg "[parent] Begin test 02"
     iv <- new
     dbg "[parent] Created IVar"

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

     (tid, cfut) <- forkCancelable p1
     dbg "[parent] Putting to IVar"
     put iv ()
     dbg "[parent] Cancelling child thread"
     cancel tid
     dbg "[parent] Issued cancel, now exiting."

-- Different than 02 in that child thread cancels itself.
case_cancel02B :: IO ()
case_cancel02B = do
  (lines, err) <- cancel02B
  assertEqual "Read an error message" False (any (isInfixOf "!!") lines)

cancel02B :: IO ([String], Either String ())
cancel02B = runDbg $ isDet $ runCancelT comp
  where
    comp :: forall p e s a .
            (HasGet e, HasPut e, HasGet (SetReadOnly e), HasPut (SetP 'P e)) =>
            CancelT Par e s ()
    comp = do
      dbg $ "Begin test 02B"

      let p1 :: CancelT Par (SetReadOnly e) s ()
          p1 = do
            dbg $ "(1) Running on child thread..."
            cancelMe
            dbg $ "!! (2) Running on child thread..."

      _ <- forkCancelable p1
      dbg $ "Waiting on main thread..."
      internalLiftIO $ appreciableDelay
      dbg $ "Now exiting on main thread."
      return ()

--------------------------------------------------------------------------------
-- Tests:

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
