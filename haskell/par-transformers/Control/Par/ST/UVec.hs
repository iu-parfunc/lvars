{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}

-- | A convenience interface -- simply a restriction of `ParST` to the case
--   of a single, boxed vector as the mutable state.
--
--   This library exposes simple versions of common operations from
--   "Data.Vector.Mutable", which operate directly on the implicit vector state
--   threaded through the monad.

module Control.Par.ST.UVec
       ( -- * A type alias for parallel computations with @Vector@ state
         ParVecT,
         runParVecT,

         -- * Reexported from the generic interface
         forkSTSplit, liftPar, liftST,

         -- * Retrieving an explict pointer to the Vector
         reify,

         -- * Common vector operations
         write, read, length, swap,
         drop, take, grow, set
       )
       where

import qualified Control.Monad.State.Strict  as S
import           Control.Par.Class.Unsafe    (ParThreadSafe)
import           Control.Par.ST
import qualified Data.Vector.Unboxed.Mutable as MU
import           Prelude                     hiding (drop, length, read, take)

--------------------------------------------------------------------------------
-- Convenience interface ONE VECTOR state:
--------------------------------------------------------------------------------

-- | A type alias for parallel computations with an unboxed `MVector` state.
type ParVecT s1 e1 p e s a = ParST (UVectorFlp e1 s1) p e s a

-- | Restricted version of `runParST` which initialized the state with a single,
-- boxed vector of a given size.  All elements start uninitialized.
runParVecT :: forall e1 p e s a . (MU.Unbox e1, ParThreadSafe p) =>
             Int
             -> (forall s1 . ParVecT s1 e1 p e s a)
             -> p e s a
runParVecT size comp =
  runParST (error "runParVec -- this initial value should be unused.") $ do
    vec <- liftST $ MU.new size
    S.put (UFlp vec)
    comp

-- | Extract a pointer to the whole Vector in its normal, usable @STVector@ form.
--   Use the `liftST` operator to act on it.
reify :: ParThreadSafe p => ParVecT s1 e1 p e s (MU.STVector s1 e1)
reify = do
  UFlp vec <- S.get
  return vec

--------------------------------------------------------------------------------

-- | Write to the (implicit) vector state.
write :: ParThreadSafe p => Int -> e1 -> ParVecT s1 e1 p e s ()
write ind val = do
  UFlp vec <- S.get
  liftST $ MU.write vec ind val

-- | Read the (implicit) vector state.
read :: ParThreadSafe p => Int -> ParVecT s1 e1 p e s e1
read ind = do
  UFlp vec <- S.get
  liftST $ MU.read vec ind

-- | Return the length of the (implicit) vector state.
length :: (ParThreadSafe p, MU.Unbox e1) => ParVecT s1 e1 p e s Int
length = S.get >>= (\(UFlp v) -> return . MU.length $ v)

-- | Update the vector state by swapping two elements.
swap :: ParThreadSafe p => Int -> Int -> ParVecT s1 e1 p e s ()
swap x y = do
  UFlp vec <- S.get
  liftST $ MU.swap vec x y

-- | Update the vector state by dropping the first @n@ elements.
drop :: ParThreadSafe p => Int -> ParVecT s1 e1 p e s ()
drop n = do
  UFlp vec <- S.get
  S.put (UFlp (MU.drop n vec))

-- | Update the vector state by taking the first @n@ elements, discarding the rest.
take :: ParThreadSafe p => Int -> ParVecT s1 e1 p e s ()
take n = do
  UFlp vec <- S.get
  S.put (UFlp (MU.take n vec))

-- | Destructively replace the vector with a bigger vector, adding the given number
-- of elements.  The new elements are uninitialized and will result in errors if
-- read.
grow :: ParThreadSafe p => Int -> ParVecT s1 e1 p e s ()
grow n = do
  UFlp vec <- S.get
  vec' <- liftST$ MU.grow vec n
  S.put (UFlp vec')

-- | Mutate all the elements of the vector, setting them to the given value.
set :: ParThreadSafe p => e1 -> ParVecT s1 e1 p e s ()
set val = do
  UFlp vec <- S.get
  liftST $ MU.set vec val
