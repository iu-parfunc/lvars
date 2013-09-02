{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}

-- | This module is NOT Safe-Haskell, but it must be used to create
-- new LVar types.
module Control.LVish.DeepFrz.Internal
       (
         DeepFrz(..), Frzn, 
       )
       where

-- | DeepFreezing is type-level (guaranteed O(1) time complexity)
-- operation.  It marks an LVar and its contents (recursively) as
-- frozen.  DeepFreezing is not an action that can be taken directly
-- by the user, however.  Rather it is an optional final-step in a
-- `runPar` invocation.
class DeepFrz a where
  -- | This type function is public.  It maps pre-frozen types to
  -- frozen ones.  It should be idempotent.
  type FrzType a :: *

  -- | Private: not exported from this module:
  frz :: a -> FrzType a

  -- | However, user's can opt-in to this class and take advantage of
  -- the default instance.
  default frz :: a -> a 
  frz a = a 


-- | An uninhabited type that signals an LVar has been frozen.
--   LVars should use this inplace of their `s` parameter.
data Frzn
