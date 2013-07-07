{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}  -- For Determinism

-- | A module that reexports the default LVish scheduler, adding some type-level
-- wrappers to ensure propert treatment of determinism.

module Control.LVish
  (
    -- * Basic types and accessors:
    LVar(), state, L.HandlerPool(), Par(), 
    Determinism(..), liftQ,
    
    -- * Safe, deterministic operations:
    yield, newPool, fork, forkInPool,
    runPar, runParIO,
    runParThenFreeze, runParThenFreezeIO,
    quiesce,
    
    -- * Interfaces for generic operations
    LVarData1(..), DeepFreeze(..),

    -- * Debug facilities
    logStrLn
  ) where

import           Control.Applicative
import           Control.LVish.Internal
import qualified Control.LVish.SchedIdempotent as L
import           System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------
-- Inline *everything*, because these are just wrappers:
{-# INLINE liftQ #-}
{-# INLINE yield #-}
{-# INLINE newPool #-}
{-# INLINE runParIO #-}
{-# INLINE runPar #-}
-- {-# INLINE runParThenFreeze #-}
{-# INLINE fork #-}
{-# INLINE forkInPool #-}
{-# INLINE quiesce #-}
--------------------------------------------------------------------------------

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

-- | If the input computation is quasi-deterministic, this may throw
-- 'NonDeterminismExn' on the thread that calls it.
runParIO :: (forall s . Par d s a) -> IO a
runParIO (WrapPar p) = L.runParIO p 

runPar :: (forall s . Par Det s a) -> a
runPar (WrapPar p) = L.runPar p 

-- | This allows Deterministic Par computations to return LVars (which normally
-- cannot escape), and it implicitly does a deepFreeze on them on their way out.
runParThenFreeze :: (DeepFreeze (f s a) b, LVarData1 f) =>
                    Par Det s (f s a) -> b
runParThenFreeze p = unsafePerformIO$ runParThenFreezeIO p

-- | This version works for quasi-deterministic computations as well.  Such
--   computations may also do freezes internally, but this function has an advantage
--   vs. doing your own freeze at the end of your computation.  Namely, when you use
--   `runParThenFreeze`, there is an implicit barrier before the final freeze.
runParThenFreezeIO :: (DeepFreeze (f s a) b, LVarData1 f) =>
                    Par d s (f s a) -> IO b
runParThenFreezeIO par@(WrapPar pi) = do 
  res <- L.runParIO pi
  runParIO (unsafeConvert $ deepFreeze res)


logStrLn :: String -> Par d s ()
logStrLn = WrapPar . L.logStrLn


------------------------------------------------------------------------------
-- Interface for generic LVar handling
------------------------------------------------------------------------------

-- class Traversable f => LVarData1 (f :: * -> *) where

-- | TODO: if there is a Par class, it needs to be a superclass of this.
class LVarData1 (f :: * -> * -> *) where
  -- | This associated type models a picture of the "complete" contents of the data:
  -- e.g. a whole set instead of one element, or the full/empty information for an
  -- IVar, instead of just the payload.
  -- type Snapshot f a :: *
  data Snapshot f :: * -> *
  
  freeze :: f s a -> Par QuasiDet s (Snapshot f a)
  newBottom :: Par d s (f s a)

  -- QUESTION: Is there any way to assert that the snapshot is still Traversable?
  -- I don't know of a good way, so instead we expose this:
  traverseSnap :: (a -> Par d s b) -> Snapshot f a -> Par d s (Snapshot f b)
  -- TODO: use constraint kinds to constrain 'b'.

  foldSnap :: (a -> b -> Par d s b) -> b -> f s a -> Par d s b

  -- What else?
  -- Merge op?

-- This gets messy if we try to handle several Kinds:
class LVarData0 (t :: *) where
  -- | This associated type models a picture of the "complete" contents of the data:
  -- e.g. a whole set instead of one element, or the full/empty information for an
  -- IVar, instead of just the payload.
  type Snapshot0 t
  freeze0 :: t -> Par QuasiDet s (Snapshot0 t)
  newBottom0 :: Par d s t

--------------------------------------------------------------------------------
-- Freezing nested structures in one go
--------------------------------------------------------------------------------

-- | This establishes an unrestricted *relation* between input and output types.  Thus
-- it is powerful, but can be painful to use.  The input and output types of
-- deepFreeze must be fully constrained at every call site.  This allows the user to
-- potentially freeze a nested structure in various ways of their choosing.
class DeepFreeze (from :: *) (to :: *) where
  type Session from 
  deepFreeze :: from -> Par QuasiDet (Session from) to

-- class DeepFreeze (from :: * -> * -> *) (to :: * -> * -> *) where
--   deepFreeze :: from s a -> Par QuasiDet s (to s a)

instance forall f g a s . (LVarData1 f, LVarData1 g) =>
         DeepFreeze (f s (g s a)) (Snapshot f (Snapshot g a)) where
  type Session (f s (g s a)) = s 
  deepFreeze lvd = unsafeConvert par
    where
      -- RRN: Type signatures here are not in the scope of the above forall... ergh.
      -- par :: Par QuasiDet s (Snapshot f (Snapshot g a))
      par = do
        x <- freeze lvd            -- :: QPar s (Snapshot f (g a))
        y <- traverseSnap freeze x -- :: QPar s (Snapshot f (Snapshot g a))
        return y

type QPar = Par QuasiDet 

-- Inherit everything that regular freeze can do:
instance LVarData1 f => DeepFreeze (f s a) (Snapshot f a) where
  type Session (f s a) = s   
  deepFreeze = unsafeConvert . freeze

