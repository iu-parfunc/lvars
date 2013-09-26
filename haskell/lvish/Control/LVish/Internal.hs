{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DataKinds #-}  -- For Determinism
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}

{-|

This module is note @SafeHaskell@, as an end-user you shouldn't ever need to import it.

It is exposed only because it is necessary for writing /new/ LVars that live in their
own, separate packages.

-}

module Control.LVish.Internal
  (
    -- * Type-safe wrappers around internal components
    Par(..), LVar(..),
    Determinism(..),
    
    -- * Unsafe conversions and lifting
    unWrapPar, unsafeRunPar,
    unsafeConvert, state,
    liftIO,

    -- * General utilities
    for_
  )
  where

import           Control.Monad.IO.Class
import           Control.LVish.MonadToss
import           Control.Applicative
import qualified Control.LVish.SchedIdempotent as L
import           Control.LVish.DeepFrz.Internal (Frzn, Trvrsbl)
import qualified Data.Foldable    as F
import           Data.List (sort)

{-# INLINE state  #-}
{-# INLINE unsafeConvert #-}
{-# INLINE unWrapPar #-}
--------------------------------------------------------------------------------

-- | This datatype is promoted to type-level and used to indicate whether a `Par`
-- computation is guaranteed-deterministic, or only quasi-deterministic (i.e. might throw NonDeterminismExn).
data Determinism = Det | QuasiDet
  deriving Show

-- | The type of parallel computations.  They may or may not be deterministic based
-- on the type arguments to `Par`.
-- 
-- Implementation note: This is a wrapper around the internal `Par` type, only with more type parameters.  Here
-- we @DataKinds@ promotion to constrain the phantom type argument to be what we
-- want.
newtype Par :: Determinism -> * -> * -> * where
  WrapPar :: L.Par a -> Par d s a
  deriving (Monad, Functor, Applicative)

-- | The generic representation of LVars used by the scheduler.  The end-user can't
-- actually do anything with these and shoud not try to.
newtype LVar s all delt = WrapLVar { unWrapLVar :: L.LVar all delt }

-- | Unsafe: drops type information to go from the safe Par monad to the internal,
-- dangerous one.
unWrapPar :: Par d s a -> L.Par a
unWrapPar (WrapPar p) = p 

-- | This is cheating!  It pays no attention to session sealing (@s@) or to the
-- determinism level (@d@).
unsafeRunPar :: Par d s a -> a
unsafeRunPar p = L.runPar (unWrapPar p)

-- | Extract the state of an LVar.  This should only be used by implementations of
-- new LVar data structures.
state :: LVar s a d -> a
state = L.state . unWrapLVar

-- | Ignore the extra type annotations regarding both determinism and session-sealing.
unsafeConvert :: Par d1 s1 a -> Par d2 s2 a
unsafeConvert (WrapPar p) = (WrapPar p)

instance MonadIO (Par d s) where
  liftIO = WrapPar . L.liftIO   

instance MonadToss (Par d s) where
  toss = WrapPar L.toss

-- My own forM for numeric ranges (not requiring deforestation optimizations).
-- Inclusive start, exclusive end.
{-# INLINE for_ #-}
for_ :: Monad m => (Int, Int) -> (Int -> m ()) -> m ()
for_ (start, end) _fn | start > end = error "for_: start is greater than end"
for_ (start, end) fn = loop start
  where
  loop !i | i == end  = return ()
          | otherwise = do fn i; loop (i+1)
  
