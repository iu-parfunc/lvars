{-# LANGUAGE Unsafe #-}

-- {-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

-- | Unsafe operations that end users should NOT import.
-- 
--   This is here only for other trusted implementation components.

module Control.Par.Class.Unsafe 
  (
   ParThreadSafe(unsafeParIO),
   ParMonad(..)
  ) 
where

-- import Control.Monad.Par.Class

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
