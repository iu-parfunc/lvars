module Data.LVar.DynMap
       where

import Data.Concurrent.SkipListMap

import Control.Concurrent
import Control.Monad
import Data.Atomics
import Data.IORef
import qualified Data.Map as M

data Hybrid k v = Pure (M.Map k v)
                | LockFree (SLMap k v)
                | Trans (SLMap k v) (M.Map k v)

type DynMap k v = IORef (Hybrid k v)

-- | The default number of skiplist levels.
defaultLevels :: Int
defaultLevels = 8

newDynMap :: IO (DynMap k v)
newDynMap = newIORef $ Pure M.empty

lookup :: Ord k => k -> DynMap k v -> IO (Maybe v)
lookup k dm = do
  val <- readIORef dm
  case val of
    Pure m -> return $ M.lookup k m
    LockFree slm -> find slm k
    Trans slm m -> do
      try <- find slm k
      case try of
        Nothing -> return $ M.lookup k m
        Just v -> return $ Just v

insert :: Ord k => DynMap k v -> k -> v -> IO ()
insert dm k v = do
  tick <- readForCAS dm
  case peekTicket tick of
    Pure m -> do
      let new = M.insert k v m -- TODO loop some number of times (n > 1) to detect contention
      (success, tick') <- casIORef dm tick $ Pure new
      unless success $ transition dm
    LockFree slm -> putIfAbsent slm k (return v) >> return ()
    Trans slm m -> do -- TODO finish
      undefined

transition :: Ord k => DynMap k v -> IO ()
transition dm = do
  h <- readIORef dm
  case h of
    Pure m -> do
      slm <- newSLMap defaultLevels
      forkIO $ do
        M.traverseWithKey (copy slm) m
        atomicWriteIORef dm $ LockFree slm
      atomicWriteIORef dm $ Trans slm m
    _ -> return ()
  where copy slm k v = putIfAbsent slm k (return v)

-- NOTES:

-- Should do latest-write-wins during transitions. This allows
-- unrestricted mutation while preserving the ability to make this an
-- LVar. Create a putOverwrite in SkipListMap for inserting into the
-- SLM.

--------------------------------------------------------------------------------

testPure :: IO (DynMap String Int)
testPure = newDynMap

testLockFree :: IO (DynMap String Int)
testLockFree = do
  slm <- newSLMap defaultLevels
  newIORef $ LockFree slm

-- test :: IO ()
test = do
  dm <- newDynMap
  forkIO $ forM_ [1..10] (\i -> forkIO $ insert dm (show i) i)
  forM_ [10..20] (\i -> forkIO $ insert dm (show i) i)
  h <- readIORef dm
  case h of
    Pure m -> print m
    LockFree slm -> (debugShow $ toSlice slm) >>= putStrLn
    Trans slm m -> print m >> (debugShow $ toSlice slm) >>= putStrLn
  return dm
