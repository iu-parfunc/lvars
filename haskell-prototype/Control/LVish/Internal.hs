{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}  -- For Determinism

module Control.LVish.Internal
  (
    Par(..), LVar(..),
    unWrapPar,
    Determinism(..), 
    unsafeConvert, state,
    liftIO
  )
  where

import           Control.Applicative
import qualified Control.LVish.SchedIdempotent as L

{-# INLINE state  #-}
{-# INLINE unsafeConvert #-}
{-# INLINE unWrapPar #-}
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

unWrapPar :: Par d s a -> L.Par a
unWrapPar (WrapPar p) = p 

state :: LVar s a d -> a
state = L.state . unWrapLVar

-- | Ignore the extra type annotations regarding both determinism and session-sealing.
unsafeConvert :: Par d1 s1 a -> Par d2 s2 a
unsafeConvert (WrapPar p) = (WrapPar p)

liftIO :: IO a -> Par d s a
liftIO = WrapPar . L.liftIO 
