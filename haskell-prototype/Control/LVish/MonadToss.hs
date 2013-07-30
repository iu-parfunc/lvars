module Control.LVish.MonadToss where

import Control.Monad
import System.Random (randomIO)

-- | A typeclass for monads supporting a coin toss operation.  NB: the coin is
-- expected to be core-local, so that flipping by multiple threads does not
-- cause contention.
class Monad m => MonadToss m where
  toss :: m Bool
  
instance MonadToss IO where  
  toss = randomIO
  -- TODO: FIXME: probably use mwc-random here instead...
