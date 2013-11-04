{-# LANGUAGE Unsafe #-}

-- {-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

-- | Unsafe operations that end users should NOT import.
-- 
--   This is here only for other trusted implementation components.

module Control.Par.Class.Unsafe 
  (
   ParThreadSafe(unsafeParIO)
  ) 
where

-- import Control.Monad.Par.Class

-- | The class of Par monads in which all monadic actions are threadsafe and do not
-- care which thread they execute on.  Thus it is ok to inject additional parallelism.
class Monad p => ParThreadSafe p where 
  -- | Run some IO in parallel on whatever thread we happen to be on.
  --   The end user does not get access to this.
  unsafeParIO :: IO a -> p a
