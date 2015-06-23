{-# LANGUAGE CPP               #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE Unsafe            #-}

-- | This module is /not/ Safe Haskell, but it must be used to create
-- new LVar types.
module Control.LVish.DeepFrz.Internal
       (
         DeepFrz(..), NonFrzn, Frzn, Trvrsbl
       )
       where

import Data.Int
import Data.Word
import GHC.Prim  (unsafeCoerce#)

-- | DeepFreezing is a type-level (guaranteed /O(1)/ time complexity)
-- operation.  It marks an LVar and its contents (recursively) as
-- frozen.  DeepFreezing is not an action that can be taken directly
-- by the user, however.  Rather, it is the final step in a
-- `runParThenFreeze` invocation.

-- An instance of DeepFrz is a valid return value for `runParThenFreeze`
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

--------------------------------------------------------------------------------

#define MKFRZINST(T) instance DeepFrz T where type FrzType T = T

MKFRZINST(Int)
MKFRZINST(Int8)
MKFRZINST(Int16)
MKFRZINST(Int32)
MKFRZINST(Int64)
MKFRZINST(Word)
MKFRZINST(Word8)
MKFRZINST(Word16)
MKFRZINST(Word32)
MKFRZINST(Word64)
MKFRZINST(Bool)
MKFRZINST(Char)
MKFRZINST(Integer)
MKFRZINST(Float)
MKFRZINST(Double)

MKFRZINST(())
MKFRZINST(Ordering)

instance DeepFrz a => DeepFrz [a] where
  type FrzType [a] = [FrzType a]
  frz = unsafeCoerce#

instance DeepFrz a => DeepFrz (Maybe a) where
  type FrzType (Maybe a) = Maybe (FrzType a)
  frz = unsafeCoerce#

instance (DeepFrz a, DeepFrz b) => DeepFrz (Either a b) where
  type FrzType (Either a b) = Either (FrzType a) (FrzType b)
  frz = unsafeCoerce#

instance (DeepFrz a, DeepFrz b) => DeepFrz (a,b) where
  type FrzType (a,b) = (FrzType a,FrzType b)
  frz = unsafeCoerce#

instance (DeepFrz a, DeepFrz b, DeepFrz c) => DeepFrz (a,b,c) where
  type FrzType (a,b,c) = (FrzType a,FrzType b,FrzType c)
  frz = unsafeCoerce#

instance (DeepFrz a, DeepFrz b, DeepFrz c, DeepFrz d) => DeepFrz (a,b,c,d) where
  type FrzType (a,b,c,d) = (FrzType a, FrzType b, FrzType c, FrzType d)
  frz = unsafeCoerce#

instance (DeepFrz a, DeepFrz b, DeepFrz c, DeepFrz d, DeepFrz e) => DeepFrz (a,b,c,d,e) where
  type FrzType (a,b,c,d,e) = (FrzType a, FrzType b, FrzType c, FrzType d, FrzType e)
  frz = unsafeCoerce#

instance (DeepFrz a, DeepFrz b, DeepFrz c, DeepFrz d, DeepFrz e,
          DeepFrz f) => DeepFrz (a,b,c,d,e,f) where
  type FrzType (a,b,c,d,e,f) = (FrzType a, FrzType b, FrzType c, FrzType d, FrzType e,
                                FrzType f)
  frz = unsafeCoerce#

instance (DeepFrz a, DeepFrz b, DeepFrz c, DeepFrz d, DeepFrz e,
          DeepFrz f, DeepFrz g) => DeepFrz (a,b,c,d,e,f,g) where
  type FrzType (a,b,c,d,e,f,g) = (FrzType a, FrzType b, FrzType c, FrzType d, FrzType e,
                                  FrzType f, FrzType g)
  frz = unsafeCoerce#


instance (DeepFrz a, DeepFrz b, DeepFrz c, DeepFrz d, DeepFrz e,
          DeepFrz f, DeepFrz g, DeepFrz h) => DeepFrz (a,b,c,d,e,f,g,h) where
  type FrzType (a,b,c,d,e,f,g,h) = (FrzType a, FrzType b, FrzType c, FrzType d, FrzType e,
                                    FrzType f, FrzType g, FrzType h)
  frz = unsafeCoerce#


instance (DeepFrz a, DeepFrz b, DeepFrz c, DeepFrz d, DeepFrz e,
          DeepFrz f, DeepFrz g, DeepFrz h, DeepFrz i) => DeepFrz (a,b,c,d,e,f,g,h,i) where
  type FrzType (a,b,c,d,e,f,g,h,i) = (FrzType a, FrzType b, FrzType c, FrzType d, FrzType e,
                                      FrzType f, FrzType g, FrzType h, FrzType i)
  frz = unsafeCoerce#
