{-# LANGUAGE Unsafe #-}
{-# LANGUAGE GADTs, RankNTypes, ConstraintKinds #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeFamilies #-}

-- {-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

-- | Unsafe operations that end users should NOT import.
-- 
--   This is here only for other trusted implementation components.

module Control.Par.Class.Unsafe 
  ( ParThreadSafe(unsafeParIO)
  , ParMonad(..)
  , ParWEffects(..)
  , ReadOnlyOf
  , ReifyConstraint(..)
  ) 
where

-- import Control.Monad.Par.Class
import Control.Par.EffectSigs
import Data.Proxy as P

import Data.Coerce

-- | The class of Par monads in which all monadic actions are threadsafe and do not
-- care which thread they execute on.  Thus it is ok to inject additional parallelism.
class Monad p => ParThreadSafe p where 
  -- | Run some IO in parallel on whatever thread we happen to be on.
  --   The end user does not get access to this.
  unsafeParIO :: IO a -> p a

-- | The essence of a Par monad is that its control flow is a binary tree of forked
-- threads.
--
-- Note, this class also serves a secondary purpose similar: providing an
-- implementation-internal way to lift IO into tho Par monad.  However, this is a
-- different use case than either `MonadIO` or `ParThreadSafe`.  Unlike the latter,
-- ALL Par monads should be a member of this class.  Unlike the former, the user
-- should not be able to access the `internalLiftIO` operation of this class from
-- @Safe@ code.
class (Functor p, Monad p) => ParMonad p where

  -- | Forks a computation to happen in parallel.  
  fork :: p () -> p ()
  
  -- | (Internal!  Not exposed to the end user.)  Lift an IO operation.  This should
  -- only be used by other infrastructure-level components, e.g. the implementation
  -- of monad transformers or LVars.
  internalLiftIO :: IO a -> p a


-- unsafeCastEffects :: ParLVar p => p a -> (SetEffects p) a

-- | A full LVar-compatible Par monad has effect tracking.
class ParWEffects m where
  -- | Type-level utility function for extracting the `e` part of a valid Par-monad stack.
  type GetEffects m :: EffectSig
  -- | Type-level utility function for replacing the `s` part of a valid Par-monad stack.
  type SetEffects (e::EffectSig) m :: (* -> *)

  -- | Effect subtyping.  Lift an RO computation to be a potentially RW one.
  liftReadOnly :: (ReadOnlyOf m) a -> m a
  
  unsafeCastEffects :: P.Proxy e -> m a -> (SetEffects e m) a
  unsafeCastEffects2 :: P.Proxy e -> (SetEffects e m) a -> m a

  -- Include evidence of a property that we might need for getting our
  -- type families to go through.
  law1 :: forall e. Proxy (m ()) -> Proxy e -> 
          ReifyConstraint (e ~ GetEffects (SetEffects e m))
  law2 :: forall e. Proxy (m ()) -> Proxy e -> 
          ReifyConstraint (ParMonad (SetEffects e m))

  coerceProp :: forall e a . 
                Proxy (m ()) -> Proxy e -> 
                ReifyConstraint (Coercible m (SetEffects e m))
  --              ReifyConstraint (Coercible (m a) ((SetEffects2 e m) a))

data ReifyConstraint c where 
  MkConstraint :: c => ReifyConstraint c

-- | A shorthand for taking the ReadOnly restriction of a given Par
-- monadic type.
--
-- This is one particular form of valid "upcast" in the implicit
-- effect subtype ordering.
type ReadOnlyOf m = (SetEffects (Ef NP (GetG (GetEffects m)) NF NB NI) m)

