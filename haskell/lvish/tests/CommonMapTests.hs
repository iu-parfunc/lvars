
-- This is NOT a full Haskell module.
-- This is a slice of source code that is #included into multiple files.

import Test.Framework.Providers.HUnit 
import Test.Framework (Test, defaultMain, testGroup)
import Test.HUnit (Assertion, assertEqual, assertBool, Counts(..))
import Test.Framework.TH (testGroupGenerator)
import qualified Test.HUnit as HU
import TestHelpers as T
import Control.Concurrent (threadDelay)
import Data.Traversable (traverse)
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.IORef
import System.Random

import           Control.LVish
import           Control.LVish.DeepFrz (DeepFrz(..), Frzn, Trvrsbl, runParThenFreeze, runParThenFreezeIO)
import qualified Control.LVish.Internal as I
import qualified Data.LVar.IVar as IV
--------------------------------------------------------------------------------


case_v7a :: Assertion
case_v7a = assertEqual "basic imap test"
--           (M.fromList [(1,1.0),(2,2.0),(3,3.0),(100,100.1),(200,201.1)]) =<<
--           [(1,1.0),(2,2.0),(3,3.0),(100,100.1),(200,201.1)] =<<
           [1.0,2.0,3.0,100.1,201.1] =<<
           v7a

-- v7a :: IO ([(Int,Float)])
v7a :: IO [Float]
v7a = fmap (L.sort . F.toList) $
  runParIO $ IM.freezeMap =<<
  do mp <- IM.newEmptyMap
     fork $ do IM.waitSize 3 mp
               IM.insert 100 100.1 mp
     fork $ do IM.waitValue 100.1 mp
               v <- IM.getKey 1 mp
               IM.insert 200 (200.1 + v) mp
     IM.insert 1 1 mp
     IM.insert 2 2 mp
     logDbgLn 1 "[v7a] Did the first two puts.."
     I.liftIO$ threadDelay 1000
     IM.insert 3 3 mp
     logDbgLn 1 "[v7a] Did the first third put."
     IM.waitSize 5 mp
     return mp


--------------------------------------------------------------------------------
-- Tests that use `forEachHP`
--------------------------------------------------------------------------------  

case_v8c :: Assertion
case_v8c = assertEqual "forEachHP on maps" [101,102] =<< v8c
           
-- | Similar test with Maps instead of Sets.
v8c :: IO [Int]
v8c = fmap (L.sort . F.toList) $
      runParIO $ do
  hp <- newPool
  m1 <- IM.newFromList [(1,1),(2,2)]
  m2 <- IM.newEmptyMap
  let cb k v = do logDbgLn 1$" [v8c]  Inside callback for Map.. key="++show k
                  IM.insert k (v+100) m2
  IM.forEachHP (Just hp) m1 cb 
  logDbgLn 1 " [v8c] Everything set up; about to quiesce..."
  quiesce hp
  logDbgLn 1 " [v8c] quiesce finished, next freeze:"
  IM.freezeMap m2


case_v8d :: Assertion
case_v8d = assertEqual "union on maps"
           [40,50,101,102] =<< v8d
v8d :: IO [Int]
v8d = fmap (L.sort . F.toList) $
      runParIO $ do
  hp <- newPool
  logDbgLn 1 " [v8d] Got a new pool..."  
  m1 <- IM.newFromList [(1,1),(2,2)]
  m2 <- IM.newFromList [(40,40),(50,50)]
  logDbgLn 1 " [v8d] Got two fresh maps..."
  let cb k v = do logDbgLn 1$" [v8d]  Inside callback for traverse.. key="++show k
                  return (v+100)
  m3 <- IM.traverseMapHP (Just hp) cb m1
  m4 <- IM.unionHP       (Just hp) m2 m3
  IM.forEachHP (Just hp) m4 $ \ k elm ->
    logDbgLn 1 $ " [v8d]   Got element: "++show (k,elm)
  logDbgLn 1 " [v8d] Everything set up; about to quiesce..."
  quiesce hp
--  quiesceAll  
  logDbgLn 1 " [v8d] quiesce finished, next freeze::"
  IM.freezeMap m4


--------------------------------------------------------------------------------
-- Issue related:
--------------------------------------------------------------------------------

-- Issue #27, spurious duplication.
case_handlrDup :: Assertion
case_handlrDup = runParIO $ do
  ctr <- I.liftIO$ newIORef 0
  mp  <- IM.newEmptyMap
  hp  <- newPool
  -- Register handler FIRST.. no race.
  IM.forEachHP (Just hp) mp $ \ (k::Int) v -> do
    logDbgLn 1 $ "[case_handlrDup] Callback executing: " ++ show (k,v)
    I.liftIO $ incr ctr
  IM.insert 2 2 mp
  IM.insert 3 3 mp 
  quiesce hp
  sum <- I.liftIO $ readIORef ctr
  I.liftIO $ assertEqual "Should be no duplication in this case" 2 sum

incr :: IORef Int -> IO ()
incr ref = atomicModifyIORef' ref (\x -> (x+1,()))

--------------------------------------------------------------------------------
-- Parallel insertion
--------------------------------------------------------------------------------

-- -- | Perform a fork-join computation and populate a SkipListMap in parallel.
-- fillOne :: [(Int, Int)] -> IO (SLM.SLMap Int Int)
-- fillOne chunks = do
--   slm <- SLM.newSLMap 10
--   mvars <- forM chunks $ \ (start,end) -> do
--     mv <- newEmptyMVar
--     forkWithExceptions forkIO "slm2 test thread" $ do
--       rgen <- newIORef $ mkStdGen 0
--       let flip = do
--             g <- readIORef rgen
--             let (b, g') = random g
--             writeIORef rgen $! g'
--             return b
--       T.for_ (start, end)$ \n -> void (SLM.putIfAbsentToss slm n (return n) flip)
--       putMVar mv ()
--     return mv  
--   forM_ mvars takeMVar  
--   return slm

-- insertionTest :: [(Int, Int)] -> IO (Bool, Word64)
-- insertionTest chunks = do
--   slm <- timeit$ fillOne chunks 
--   -- End timing.  Timing just the insertion phase.
--   cs <- SLM.counts slm
--   logDbgLn_ 1 $ "After insertions, counts: " ++ show cs
--   sliceCheck slm    
--   matches <- SLM.foldlWithKey id (\b k v -> if k == v then return b else return False) True slm
--   summed  <- SLM.foldlWithKey id (\s _ v -> return $! s + fromIntegral v) 0 slm
--   printLog
--   return (matches, summed)



--------------------------------------------------------------------------------
-- Parallel folding  
--------------------------------------------------------------------------------

-- case_parfoldslm1 :: Assertion 
-- case_parfoldslm1 =
--   assertEqual "test concurrent insertion for SkipListMap (#4)" expectedSum =<<
--     (do slm <- fillOne (splitRange numCapabilities (1,mediumSize))
--         return expectedSum
--     )



--------------------------------------------------------------------------------

tests_common :: Test
tests_common = testGroup "Common" [ $(testGroupGenerator) ] 
