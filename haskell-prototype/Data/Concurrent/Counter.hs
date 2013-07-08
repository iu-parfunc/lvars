module Data.Concurrent.Counter(Counter, new, inc, dec, poll) where

import Control.Monad
import Control.Concurrent
import Data.IORef
import Data.IntSet as S
import Data.Word

-- | Keeps a counter for coining new unique IDs, and a set of active IDs.
type Counter = IORef (Int, S.IntSet)
type UID = Int

new :: IO Counter
new = newIORef (0,S.empty)

inc :: Counter -> IO UID
inc c = atomicModifyIORef c $ \ (n,s) -> ((n+1, S.insert n s),n)

dec :: Counter -> UID -> IO ()
dec c id = atomicModifyIORef c $ \(n,s) -> ((n, S.delete id s),())

-- | Is the counter (transiently) zero?
poll :: Counter -> IO Bool
poll c = do
  (_,s) <- readIORef c
  return (S.size s == 0) 
