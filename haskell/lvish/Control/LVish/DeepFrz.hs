{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

{-|

Provides a way to return arbitrarily complex data-structures containing LVars
from `Par` computations.

The important thing to know is that to use `runParThenFreeze` you must make sure that
all types you return from the parallel computation have `DeepFrz` instances.  This
means if you introduce custom (non-LVar) datatypes, you will need to include a bit of
boilerplate to give them `DeepFrz` instances.  Here is a complete example:

> {-# LANGUAGE TypeFamilies #-}
> import Control.LVish.DeepFrz
> 
> data MyData = MyData Int deriving Show
> 
> instance DeepFrz MyData where
>   type FrzType MyData = MyData
> 
> main = print (runParThenFreeze (return (MyData 3)))

-}

module Control.LVish.DeepFrz
       (
         -- * The functions you'll want to use
         runParThenFreeze,
         runParThenFreezeIO,

         -- * Some supporting types
         DeepFrz(), FrzType,
         Frzn, Trvrsbl,
         
       ) where

import Data.Int
import Data.Word
import GHC.Prim (unsafeCoerce#)

-- import Control.LVish (LVarData1(..))
import Control.LVish.DeepFrz.Internal (DeepFrz(..), Frzn, Trvrsbl)
import Control.LVish.Internal (Determinism(..), Par(WrapPar))
import Control.LVish.SchedIdempotent (runPar, runParIO)
--------------------------------------------------------------------------------

-- | Under normal conditions, calling a `freeze` operation makes a `Par` computation
-- quasi-deterministic.  However, if we freeze once all LVar operations are completed
-- (after the implicit global barrier of `runPar`), then we've avoided all data
-- races, and freezing is therefore safe.
-- 
-- For this to be possible, the type returned from the Par computation must be a
-- member of the `DeepFrz` class.  All LVar libraries should provide this instance
-- already.  Further, you can create additional instances for custom, pure datatypes.
-- The result of a `runParThenFreeze` depends on the type-level function `FrzType`,
-- whose only purpose is to toggle the `s` parameters of all IVars to the `Frzn`
-- state.
--
-- Significantly, the freeze at the end of `runParThenFreeze` has NO runtime cost, in
-- spite of the fact that it enables a DEEP (recursive) freeze of the value returned
-- by the `Par` computation.
runParThenFreeze :: DeepFrz a => Par Det s a -> FrzType a
runParThenFreeze (WrapPar p) = frz $ runPar p

-- | This version works for non-deterministic computations as well.
-- 
-- Such computations may also do freezes internally, but this function has an
-- advantage vs. doing your own freeze at the end of your computation.  Namely, when
-- you use `runParThenFreezeIO`, there is an implicit barrier before the final
-- freeze.  Further, DeepFrz has no runtime overhead, whereas regular freezing has a cost.
runParThenFreezeIO :: DeepFrz a => Par d s a -> IO (FrzType a)
runParThenFreezeIO (WrapPar p) = do
  x <- runParIO p
  return $ frz x

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
