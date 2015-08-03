{-# LANGUAGE Unsafe            #-}

{-# LANGUAGE CPP               #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

-- {-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Unsafe operations that end users should NOT import.
--
--   This is here only for other trusted implementation components.

module Control.Par.Class.Unsafe
  ( ParMonad(..)
  , ParFuture(..)
  , IdempotentParMonad
  , LazyFutures, EagerFutures
  , ParThreadSafe

  , SecretSuperClass
  )
where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Control.DeepSeq
import Control.Par.EffectSigs
import Unsafe.Coerce (unsafeCoerce)


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
      -- TODO: Switch to new safe coercion framework (e.g. Eisenberg roles work)

  -- | Effect subtyping.  Lift an RO computation to be a potentially RW one.
  liftReadOnly :: p (SetReadOnly e) s a -> p e s a
  liftReadOnly = unsafeCoerce

-- | If we use the current design for ParMonad, we suffer these orphan
-- instances: (We cannot include Monad as a super-class of ParMonad,
-- because it would have to universally quantify over 'e' and 's',
-- which is not allowed.)
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


--------------------------------------------------------------------------------

-- | @ParFuture@ captures the class of Par monads which support
--   futures.  This level of functionality subsumes @par@/@pseq@ and is
--   similar to the "Control.Parallel.Strategies.Eval" monad.
--
--   A minimal implementation consists of `spawn_` and `get`.
--   However, for monads that are also a member of `ParIVar` it is
--   typical to simply define `spawn` in terms of `fork`, `new`, and `put`.
--
--   All implementations should meet the following two laws.  First,
--   the spawn-reordering law:
--
--   > (do x <- spawn f; y <- spawn g; m) ==
--   > (do y <- spawn g; x <- spawn f; m)
--
--   Second, the spawn/read identity:
--
--   > (spawn_ m >>= read)  ==  (m >>= evaluate)
--
--   Where `evaluate` would be analogous to
--   `Control.Exception.evaluate`, and would capture the fact that `spawn_`
--  evaluates to WHNF.
class ParMonad m => ParFuture (m :: EffectSig -> * -> * -> *) where
  {-# MINIMAL spawn_, read #-}

  -- | The type of a future that goes along with the particular `Par`
  -- monad the user chooses.  The future is parameterized by the 's' param as well.
  type Future m :: * -> * -> *

  -- | Create a potentially-parallel computation, and return a /future/
  -- (or /promise/) that can be used to query the result of the forked
  -- computataion.
  --
  -- >  spawn p = do
  -- >    r <- new
  -- >    fork (p >>= put r)
  -- >    return r
  --
  spawn  :: (NFData a) => m e s a -> m e s (Future m s a)
  spawn  p = spawn_ (do x <- p; deepseq x (return x))

  -- | Like 'spawn', but the result is only head-strict, not fully-strict.
  spawn_ :: m e s a -> m e s (Future m s a)

  -- | Wait for the result of a future, and then return it.  This is a
  --   blocking operation, but only in the bounded sense that it takes
  --   time to compute the value.  Either the current thread will work
  --   on the result, or another thread is already working on the
  --   result.
  read   :: (HasGet e) => Future m s a -> m e s a

  -- | Spawn a pure (rather than monadic) computation.  Fully-strict.
  --
  -- >  spawnP = spawn . return
  spawnP :: (NFData a) => a -> m e s (Future m s a)
  spawnP a = spawn (return a)

  -- default spawn_ :: (ParIVar m, FutContents m a) => m a -> m (Future m a)
  -- spawn_ p = do r <- new;  fork (p >>= put_ r);  return r

--------------------------------------------------------------------------------
-- Empty classes, futures

-- | This type class provides no methods.  Rather, it documents a
-- semantic property of a particular monad with futures.  Any monad
-- with this property must reveal divergence or exceptions within
-- futures only lazily, when the future is read.  Thus:
--
-- > (spawn loop >> m) == m
-- > (spawn err  >> m) == m
--
-- Where:
--
-- > loop = return () >> loop
-- > err  = return () >> undefined
--
class (SecretSuperClass m, ParFuture m) => LazyFutures m where
-- class ParFuture m => ForgettableFutures m where

-- | This type class provides no methods.  Rather, it documents a
-- semantic property of a particular monad with futures.  Any monad
-- with this property must wait until all spawned futures are complete
-- before exitting a parallel session.  Further, it must ensure that
-- all spawned futures terminate without exception, before the
-- parallel session may terminate without exception.  This implies a
-- global barrier at the end of each parallel session.
--
-- All implementations should satisfy the law:
--
-- > (spawn err  >> m) == err  || otherError
-- > (spawn loop >> m) == loop || otherError
--
-- Here `OtherError` corresponds to the scenario where `m` throws a
-- different exception.  Here we can see imprecise exceptions in
-- action; WHICH exception we get on a particular run may be
-- nondeterministic with Par monad executions in general, which in
-- turn leverages Haskell's standard notion of imprecise exceptions.
class (SecretSuperClass m, ParFuture m) => EagerFutures m where
-- class ParFuture m => UnforgettableFutures m where
