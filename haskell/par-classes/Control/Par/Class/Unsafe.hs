{-# LANGUAGE CPP               #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE Unsafe            #-}

-- {-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

-- | Unsafe operations that end users should NOT import.
--
--   This is here only for other trusted implementation components.

module Control.Par.Class.Unsafe
  ( ParMonad(..)
  , IdempotentParMonad
  , ParThreadSafe

  , SecretSuperClass
  )
where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Control.Par.EffectSigs

import Unsafe.Coerce          (unsafeCoerce)

-- | The essence of a Par monad is that its control flow is a binary tree of forked
-- threads.
--
-- Note, this class also serves a secondary purpose similar: providing an
-- implementation-internal way to lift IO into tho Par monad.  However, this is a
-- different use case than either `MonadIO` or `ParThreadSafe`.  Unlike the latter,
-- ALL Par monads should be a member of this class.  Unlike the former, the user
-- should not be able to access the `internalLiftIO` operation of this class from
-- @Safe@ code.
class ParMonad (p :: EffectSig -> * -> * -> *)
  where
  pbind :: p e s a -> (a -> p e s b) -> p e s b
  preturn :: a -> p e s a

  -- | Forks a computation to happen in parallel.
  fork :: p e s () -> p e s ()

  -- | (Internal!  Not exposed to the end user.)  Lift an IO operation.  This should
  -- only be used by other infrastructure-level components, e.g. the implementation
  -- of monad transformers or LVars.
  internalLiftIO :: IO a -> p e s a

  -- | (Internal! Not exposed to the end user.) Unsafely cast effect signatures.
  internalCastEffects :: p e1 s a -> p e2 s a
  internalCastEffects = unsafeCoerce

  -- | Effect subtyping.  Lift an RO computation to be a potentially RW one.
  liftReadOnly :: p (SetReadOnly e) s a -> p e s a
  liftReadOnly = unsafeCoerce

-- If we use this design for ParMonad, we suffer these orphan instances:
-- (We cannot include Monad as a super-class of ParMonad, because it would
--  have to universally quantify over 'e' and 's', which is not allowed.)
instance ParMonad p => Monad (p e s) where
  (>>=) = pbind
  return = preturn

instance ParMonad p => Functor (p e s) where
  fmap f p = pbind p (return . f)

instance ParMonad p => Applicative (p e s) where
  pure = preturn
  f <*> x = pbind f (\f' -> pbind x (return . f'))


--------------------------------------------------------------------------------
-- Empty classes, ParMonad

-- | This empty class is ONLY present to prevent users from instancing
-- classes which they should not be allowed to instance within the
-- SafeHaskell-supporting subset of parallel programming
-- functionality.
class SecretSuperClass (p :: EffectSig -> * -> * -> *) where

-- | This type class denotes the property that:
--
-- > (m >> m) == m
--
-- For all actions `m` in the monad.  For example, any concrete Par
-- monad which implements *only* `ParFuture` and/or `ParIVar`, would
-- retain this property.  Conversely, any `NonIdemParIVar` monad would
-- violate the property.
class (SecretSuperClass p, ParMonad p) => IdempotentParMonad p where


-- | The class of Par monads in which all monadic actions are threadsafe and do not
-- care which thread they execute on.  Thus, it is ok to inject additional parallelism.
--
-- Specifically, instances of ParThreadSafe must satisfy the law:
--
-- > (do m1; m2) == (do fork m1; m2)
--
class (SecretSuperClass p, ParMonad p) => ParThreadSafe (p :: EffectSig -> * -> * -> *) where
