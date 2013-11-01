{-# LANGUAGE TemplateHaskell #-}

module CancelTests where

import Control.LVish
import Control.LVish.CancelT
-- import Control.Par.Class
import Control.LVish.Unsafe
import Data.LVar.IVar

import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Trans

import Test.HUnit (Assertion, assert, assertEqual, assertBool, Counts(..))
-- import Test.QuickCheck ()
import Test.Framework.Providers.HUnit
-- import Test.Framework.Providers.QuickCheck2
import Test.Framework -- (Test, defaultMain, testGroup)
import Test.Framework.TH (testGroupGenerator)

--------------------------------------------------------------------------------

-- case_cancel01 =

-- type MyM a = CancelT (Par d s)

-- | This deadlocks, because the last computation was canceled!
cancel01 = runParIO $ runCancelT $ do
  liftIO $ putStrLn "Begin test 01"
  cancelMe
  liftIO $ putStrLn "Past cancelation point!"
  return ()


-- | This should always cancel the child before printing "!!".
cancel02 = runParIO $ runCancelT $ do
  liftIO $ putStrLn "[parent] Begin test 02"
  iv  <- lift new
  tid <- forkCancelable $ do
           liftIO $ putStrLn "[child] Running on child thread... block so parent can run"
           -- lift $ Control.LVish.yield -- Not working!
           lift$ get iv -- This forces the parent to get scheduled.
           liftIO $ putStrLn "[child] Woke up, now wait so we will be cancelled..."
           liftIO $ appreciableDelay
           pollForCancel
           liftIO $ putStrLn "!! [child] thread got past delay!"
  lift$ put iv ()
  cancel tid
  liftIO $ putStrLn "[parent] Issued cancel, now exiting."
  return ()

cancel03 = runParIO $ runCancelT $ do
  liftIO $ putStrLn "Begin test 03"
  tid <- forkCancelable $ do
      liftIO$ putStrLn "(1) Running on child thread..."
      cancelMe
      liftIO$ putStrLn "(2) Running on child thread..."

  liftIO $ putStrLn "Waiting on main thread..."
  liftIO $ appreciableDelay
  liftIO $ putStrLn "Now exiting on main thread."
  return ()


appreciableDelay = threadDelay (100 * 1000)


main = cancel02
