{-# LANGUAGE DataKinds #-}  -- For Determinism
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A module that reexports the default LVish scheduler, adding some type-level
-- wrappers to ensure propert treatment of determinism.

module Control.LVish
  (
    -- * Basic types and accessors:
    LVar(WrapLVar), state, L.HandlerPool(), Par(WrapPar), 
    Determinism(..), liftQ,
    -- NOTE: It is safe to export WrapPar, because without importing the Internal
    -- SchedIdempotent module, a client cannot do anything with it.
    
    -- * Safe, deterministic operations:
    yield, newPool, fork, forkInPool,
    runPar, runParIO, runParThenFreeze,
    quiesce,
    
    -- * Interfaces for generic operations
    L.LVarData1(..), L.DeepFreeze(..),

    -- * Debug facilities
    logStrLn
  ) where

import           Control.Applicative
import qualified Control.LVish.SchedIdempotent as L
import           System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------

-- | This datatype is promoted to type-level and used to indicate whether a `Par`
-- computation is guaranteed-deterministic, or only quasi-deterministic (i.e. might throw NonDeterminismExn).
data Determinism = Det | QuasiDet
  deriving Show

-- Use DataKinds promotion to constrain the phantom type argument to be what we want.
newtype Par :: Determinism -> * -> * -> * where
  WrapPar :: L.Par a -> Par d s a
  deriving (Monad, Functor, Applicative)
-- type Foo = Par Det -- This is fine.
-- type Bar = Par Int -- Nice type error for this.


newtype LVar s all delt = WrapLVar { unWrapLVar :: L.LVar all delt }

state :: LVar s a d -> a
state = L.state . unWrapLVar

-- unsafeConvert :: Par d1 s a -> Par d2 s a
-- unsafeConvert (WrapPar p) = (WrapPar p)

-- | It is always safe to lift a deterministic computation to a quasi-determinism one.
liftQ :: Par Det s a -> Par QuasiDet s a
liftQ (WrapPar p) = (WrapPar p)

yield :: Par d s ()
yield = WrapPar L.yield

quiesce :: L.HandlerPool -> Par d s ()
quiesce = WrapPar . L.quiesce

fork :: Par d s () -> Par d s ()
fork (WrapPar f) = WrapPar$ L.fork f

forkInPool :: L.HandlerPool -> Par d s () -> Par d s ()
forkInPool hp (WrapPar f) = WrapPar$ L.forkInPool hp f

newPool :: Par d s L.HandlerPool
newPool = WrapPar L.newPool

runParIO :: (forall s . Par d s a) -> IO a
runParIO (WrapPar p) = L.runParIO p 

runPar :: (forall s . Par Det s a) -> a
runPar (WrapPar p) = L.runPar p 

-- | This allows Deterministic Par computations to return LVars (which normally
-- cannot escape), and it implicitly does a deepFreeze on them on their way out.
runParThenFreeze :: (L.DeepFreeze (f a) b, L.LVarData1 f) =>
                    Par Det s (f a) -> b
runParThenFreeze (WrapPar p) = unsafePerformIO $
                               L.runParThenFreezeIO p

-- | This version works for quasi-deterministic computations as well.  Such
--   computations may also do freezes internally, but this function has an advantage
--   vs. doing your own freeze at the end of your computation.  Namely, when you use
--   `runParThenFreeze`, there is an implicit barrier before the final freeze.
runParThenFreezeIO :: (L.DeepFreeze (f a) b, L.LVarData1 f) =>
                    Par d s (f a) -> IO b
runParThenFreezeIO (WrapPar p) = L.runParThenFreezeIO p

logStrLn :: String -> Par d s ()
logStrLn = WrapPar . L.logStrLn

--------------------------------------------------------------------------------
-- Inline *everything*, because these are just wrappers:

{-# INLINE liftQ #-}
{-# INLINE yield #-}
{-# INLINE newPool #-}
{-# INLINE runParIO #-}
{-# INLINE runPar #-}
