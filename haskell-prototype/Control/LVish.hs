
-- | A module that reexports the default LVish scheduler.

module Control.LVish
  (
    -- * Basic types and accessors:
    LVar(), state, HandlerPool(), Par(), QPar(), liftQ,
    
    -- * Safe, deterministic operations:
    yield, newPool, fork, forkInPool,
    runPar, runParIO, runParThenFreeze,
    
    -- newLV, getLV, putLV, freezeLV, freezeLVAfter,
    -- addHandler, liftIO, 
    
    -- * Quasi-deterministic operations:
    quiesce, runQParIO,

    -- * Interfaces for generic operations
    LVarData1(..), DeepFreeze(..),

    -- * Debug facilities
    logStrLn
  ) where

import Control.LVish.SchedIdempotent
