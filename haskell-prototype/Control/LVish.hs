
-- | A module that reexports the default LVish scheduler.

module Control.LVish
  (
    -- * Basic types and accessors:
    LVar(), state, HandlerPool(), Par(),

    
    -- * Safe, deterministic operations:
    yield, newPool, fork, forkInPool,
    runParIO,
    -- runPar, runQParIO
    
    -- newLV, getLV, putLV, freezeLV, freezeLVAfter,
    -- addHandler, liftIO, 
    
    -- * Quasi-deterministic operations:
    quiesce,

    -- * Interfaces for generic operations
    LVarData1(..),

    -- * Debug facilities
    logStrLn

  ) where

import Control.LVish.SchedIdempotent
