{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module CancelTests (tests, runTests)
       where

import Control.LVish            (isDet, runParDetailed, runParNonDet)
import Control.LVish            (DbgCfg (..))
import Control.LVish.CancelT
import Control.LVish.Internal
import Control.Par.Class
import Control.Par.Class.Unsafe
import Control.Par.EffectSigs

import Control.Concurrent
import Data.IORef
import Data.List                (isInfixOf)
import Prelude                  hiding (log)

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "CancelT tests"
  [simpleLogging, loggingWCancelT, cancelSelfSimple,
   cancelSelf, cancelOther, cancelOther_self]

runTests :: IO ()
runTests = defaultMain tests

--------------------------------------------------------------------------------
-- Helpers:

appreciableDelay :: IO ()
appreciableDelay = threadDelay (100 * 1000)

type Logger = IORef [String] -- new logs are just consed, reverse before use

dbg :: ParMonad p => Logger -> String -> p e s ()
dbg logger msg = internalLiftIO $ modifyIORef' logger (msg :)

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

assertNoError :: [String] -> Assertion
assertNoError logs =
  assertBool ("Found error message in logs: " ++ show logs) (not $ any (isInfixOf "!!") logs)

-- | A runner for testing which both grabs logs AND echos them to the screen.
runDbg :: (Logger -> forall s . Par e s a) -> IO ([String], Either String a)
runDbg comp = do
  logs <- newIORef []
  numCap <- getNumCapabilities
  (_,ans) <- runParDetailed
                   DbgCfg { dbgRange = Nothing
                          , dbgDests = []
                          , dbgScheduling = False }
                   numCap (comp logs)
  logs' <- reverse `fmap` readIORef logs
  case ans of
    Left err  -> return $! (logs', Left (show err))
    Right x   -> return $! (logs', Right x)

simpleLogging :: TestTree
simpleLogging = testCase "Make sure the logger is working with Par" $ do
  let logMsg = "Testing the logger."
  (logs, _) <- runDbg $ \l -> dbg l logMsg
  assertHasLog logMsg logs

-- Make sure logging is working with CancelT.
loggingWCancelT :: TestTree
loggingWCancelT = testCase "Make sure logging is working with CancelT" $ do
  let logMsg = "Testing the logger."
  (logs, _) <- runDbg $ \l -> isDet $ runCancelT $ dbg l logMsg
  assertHasLog logMsg logs

-- Make sure `cancelMe` is not crashing anything. (no logging here)
cancelSelfSimple :: TestTree
cancelSelfSimple = testCase "Make sure `cancelMe` is not crashing anything" $ do
  runParNonDet $ runCancelT cancelMe

cancelSelf :: TestTree
cancelSelf = testCase "Self cancellation with logging" $ do
  logs <- cancelSelf'
  assertHasLog "Begin test 01" logs
  assertNoError logs

cancelSelf' :: IO [String]
cancelSelf' = do
  (logs, Left err) <- runDbg $ \l -> isDet $ runCancelT $ do
       dbg l "Begin test 01: about to cancelMe on main thread"
       cancelMe -- this is just returnToSched from LVarSched method of Par
       dbg l "!! Past cancelation point!"
  assertBool "cancel01: correct error" $ "cancelMe: cannot be used" `isInfixOf` err
  return logs

cancelOther :: TestTree
cancelOther = testCase "Cancel other thread" $ cancelOther' >>= assertNoError . fst

-- | This should always cancel the child before printing "!!".
cancelOther' :: IO ([String], Either String ())
cancelOther' =
  runDbg $ \l -> isDet $ runCancelT (comp l)
 where
   comp :: forall e s .
           (HasGet e, HasPut e, HasGet (SetReadOnly e), HasPut (SetP 'P e)) =>
           Logger -> CancelT Par e s ()
   comp l = do
     dbg l "[parent] Begin test 02"
     iv <- new
     dbg l "[parent] Created IVar"

     let p1 :: CancelT Par (SetReadOnly e) s ()
         p1 = do
           dbg l "[child] Running on child thread... block so parent can run"
           -- lift $ Control.LVish.yield -- Not working!
           -- Do we need this? ^ Next line will force rescheduling.
           get iv -- This forces the parent to get scheduled.
           dbg l "[child] Woke up, now wait so we will be cancelled..."
           internalLiftIO appreciableDelay
           pollForCancel
           dbg l "!! [child] thread got past delay!"

     (tid, _) <- forkCancelable p1
     dbg l "[parent] Putting to IVar"
     put iv ()
     dbg l "[parent] Cancelling child thread"
     cancel tid
     dbg l "[parent] Issued cancel, now exiting."

-- Different than 02 in that child thread cancels itself.
cancelOther_self :: TestTree
cancelOther_self = testCase "Spawned thread cancels itself" $ do
  (logs, _) <- cancelOther_self'
  assertNoError logs

cancelOther_self' :: IO ([String], Either String ())
cancelOther_self' = runDbg $ \l -> isDet $ runCancelT (comp l)
  where
    comp :: forall e s .
            (HasGet e, HasPut e, HasGet (SetReadOnly e), HasPut (SetP 'P e)) =>
            Logger -> CancelT Par e s ()
    comp l = do
      dbg l "Begin test 02B"

      let p1 :: CancelT Par (SetReadOnly e) s ()
          p1 = do
            dbg l "(1) Running on child thread..."
            cancelMe
            dbg l "!! (2) Running on child thread..."

      _ <- forkCancelable p1
      dbg l "Waiting on main thread..."
      internalLiftIO $ appreciableDelay
      dbg l "Now exiting on main thread."
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
