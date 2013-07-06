
-- | A module that reexports the default LVish scheduler.

module Control.LVish
  (
    -- * Basic types and accessors:
    LVar(), state, HandlerPool(), Par(), QPar(), liftQ,
    
    -- * Safe, deterministic operations:
    yield, newPool, fork, forkInPool,
    runPar, runParIO,
    
    -- newLV, getLV, putLV, freezeLV, freezeLVAfter,
    -- addHandler, liftIO, 
    
    -- * Quasi-deterministic operations:
    quiesce, runQParIO,

    -- * Interfaces for generic operations
    LVarData1(..),

    -- * Debug facilities
    logStrLn
  ) where

import Control.LVish.SchedIdempotent


class Monad p => LParMonad p where
  -- type HandlerPool p
  -- type LVar
  -- state 
  -- yield, newPool, fork, forkInPool,
  
  -- addHandler
  
-- class QParMonad q where
  
instance LParMonad Par where

instance LParMonad QPar where

  
{-
class Monad m => ParFuture m where
  type Future m a
  
class ParFuture m => ParIVar m where
  type IVar m a

class LParSet m where
  type ISet m a
  

-}
