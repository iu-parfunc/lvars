{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FlexibleInstances #-}

module Control.LVish.DeepFrz
       (
         DeepFrz(), FrzType,
         Frzn,
       ) where

import Data.Int
import Data.Word
import GHC.Prim (unsafeCoerce#)

-- import Control.LVish (LVarData1(..))
import Control.LVish.DeepFrz.Internal
--------------------------------------------------------------------------------

{-
-- This won't work because it conflicts with other instances such as "Either":
instance (LVarData1 f, DeepFrz a) => DeepFrz (f s a) where
  type FrzType (f s a) = f Frzn (FrzType a)
  frz = unsafeCoerce#
-}

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

