{-# LANGUAGE Unsafe #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}  -- For Determinism
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}


{-|

This module is /not/ Safe Haskell; as an end-user, you shouldn't ever
need to import it.

It is exposed only because it is necessary for implementing /new/ LVar
types that will live in their own, separate packages.

-}

module Control.LVish.Internal
  (
    -- * Type-safe wrappers around internal components
    Par(..), LVar(..),
    
    -- * Unsafe conversions and lifting
    unWrapPar, unsafeRunPar,
    unsafeConvert, unsafeDet,
    state, liftIO,

    -- * Debugging information taken from the environment
    L.dbgLvl
  )
  where

import           Control.LVish.MonadToss
import           Control.Applicative
import qualified Internal.Control.LVish.Sched as L
import           Control.LVish.DeepFrz.Internal (Frzn, Trvrsbl)
import           Control.Par.EffectSigs

import qualified Data.Foldable    as F
import           Data.List (sort)

{-# INLINE state  #-}
{-# INLINE unsafeConvert #-}
{-# INLINE unWrapPar #-}
--------------------------------------------------------------------------------

-- | The type of parallel computations.  A computation @Par e s a@ may or may not be
-- deterministic based on the setting of the `d` parameter (of kind `Determinism`).
-- The `s` parameter is for preventing the escape of @LVar@s from @Par@ computations
-- (just like the @ST@ monad).  
-- 
-- Implementation note: This is a wrapper around the internal `Par` type, only with more type parameters.  
newtype Par :: EffectSig -> * -> * -> * where
  WrapPar :: L.Par a -> Par e s a
  deriving (Monad, Functor, Applicative)

-- type instance GetSession (Par e s) = s
--- type instance SetSession s2 (Par e s) = Par e s2

-- Example type error:
-- _ = foo bar
-- foo :: Deterministic e => Par e s a -> Int
-- foo = undefined
-- bar :: Par (Ef P G F B I) s Int
-- bar = undefined




-- | The generic representation of LVars used by the scheduler.  The
-- end-user can't actually do anything with these and should not try
-- to.

-- LK: I don't care if we use `a` and `d` or `all` and `delt`, but why
-- not be consistent between here and Sched.hs?  Also, what
-- does `all` mean?
newtype LVar s all delt = WrapLVar { unWrapLVar :: L.LVar all delt }

-- | Unsafe: drops type information to go from the safe `Par` monad to
-- the internal, dangerous one.
unWrapPar :: Par e s a -> L.Par a
unWrapPar (WrapPar p) = p 

-- | This is cheating!  It pays no attention to session sealing (@s@) or to the
-- determinism level (@d@).
unsafeRunPar :: Par e s a -> a
unsafeRunPar p = L.runPar (unWrapPar p)

-- | Extract the state of an LVar.  This should only be used by implementations of
-- new LVar data structures.
state :: LVar s a d -> a
state = L.state . unWrapLVar

-- | Ignore the extra type annotations regarding both determinism and session-sealing.
unsafeConvert :: Par e1 s1 a -> Par e2 s2 a
unsafeConvert (WrapPar p) = (WrapPar p)

-- | Unsafe coercion from quasi-deterministic to deterministic.  The user is
-- promising that code is carefully constructed so that put/freeze races will not
-- occur.
unsafeDet :: Par e1 s a -> Par e2 s a
unsafeDet (WrapPar p) = (WrapPar p)

instance MonadToss (Par e s) where
  toss = WrapPar L.toss

-- | Unsafe internal operation to lift IO into the Par monad.
liftIO :: IO a -> Par e s a
liftIO = WrapPar . L.liftIO   
