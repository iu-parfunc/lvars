module Data.Concurrent.Counter(Counter, new, inc, dec, poll) where

import Control.Monad
import Control.Concurrent
import Data.IORef

type Counter = IORef Int

new :: IO Counter
new = newIORef 0

inc :: Counter -> IO ()
inc c = atomicModifyIORef c $ \n -> (n+1,())

dec :: Counter -> IO ()
dec c = atomicModifyIORef c $ \n -> (n-1,())

-- | Is the counter (transiently) zero?
poll :: Counter -> IO Bool
poll = error "todo"