-- | A simple, non-scalable counter.

module Data.Concurrent.Counter(Counter, new, inc, dec, poll) where

import Data.IORef

type Counter = IORef Int

new :: IO Counter
new = newIORef 0

-- TODO: at least switch to use fetch-and-add...
inc :: Counter -> IO ()
inc c = atomicModifyIORef' c $ \n -> (n+1,())

dec :: Counter -> IO ()
dec c = atomicModifyIORef' c $ \n -> (n-1,())

-- | Is the counter (transiently) zero?
poll :: Counter -> IO Bool
poll c = do
  n <- readIORef c
  return (n == 0)
