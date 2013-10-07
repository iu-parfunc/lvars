{-# LANGUAGE Unsafe #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE EmptyDataDecls #-}

-- | This module is /not/ Safe Haskell, but it must be used to create
-- new LVar types.
module Control.LVish.DeepFrz.Internal
       (
         DeepFrz(..), NonFrzn, Frzn, Trvrsbl 
       )
       where

-- | DeepFreezing is a type-level (guaranteed /O(1)/ time complexity)
-- operation.  It marks an LVar and its contents (recursively) as
-- frozen.  DeepFreezing is not an action that can be taken directly
-- by the user, however.  Rather, it is the final step in a
-- `runParThenFreeze` invocation.

-- An instance of DeepFrz is a valid return valud for `runParThenFreeze`
class DeepFrz a where
  -- | This type function is public.  It maps pre-frozen types to
  -- frozen ones.  It should be idempotent.
  type FrzType a :: *

  -- | Private: not exported to the end user.
  frz :: a -> FrzType a

  -- | While `frz` is not exported, users may opt-in to the `DeepFrz`
  -- class for their datatypes and take advantage of the default instance.
  -- Doing so REQUIRES that `type FrzType a = a`.
  default frz :: a -> a 
  frz a = a 

-- | An uninhabited type that signals that an LVar has been frozen.
-- LVars should use this in place of their `s` parameter.
data Frzn

-- | This exists only for the purpose of being a type which is /not/ equal to `Frzn`.
-- One could just as well have used @()@, but this is more descriptive.
data NonFrzn

-- | An uninhabited type that signals that an LVar is not only frozen,
-- but it may be traversed in whatever order its internal
-- representation dictates.
data Trvrsbl 

