{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

-- | A convenience interface -- simply a restriction of `ParST` to the case
--   of a single, boxed vector as the mutable state.
--
--   This library exposes simple versions of common operations from
--   "Data.Vector.Mutable", which operate directly on the implicit vector state
--   threaded through the monad.

module Control.LVish.ST.UVec
       ( -- * A type alias for parallel computations with @Vector@ state
         ParVec, 
         runParVec, runParVec',

         -- * Reexported from the generic interface
         forkWithVec, liftPar, liftST, 
         
         -- * Retrieving an explict pointer to the Vector
         reify, liftST,
                
         -- * Common vector operations
         write, read, length, swap,
         drop, take, grow, set
       )
       where

import Control.LVish (Par, Determinism(Det), runPar)
import Control.LVish.ST
import qualified Control.Monad.State.Strict as S
import qualified Data.Vector.Unboxed.Mutable as MU
import Prelude hiding (read, length, drop, take)

--------------------------------------------------------------------------------
-- Convenience interface ONE VECTOR state:
--------------------------------------------------------------------------------

-- | A type alias for parallel computations with an unboxed `MVector` state.
type ParVec s1 elt det s2 ans = ParST (UVectorFlp elt s1) det s2 ans

-- | Restricted version of `runParST` which initialized the state with a single,
-- boxed vector of a given size.  All elements start uninitialized.
runParVec :: forall elt s2 det ans . (MU.Unbox elt) =>
             Int
             -> (forall s1 . ParVec s1 elt det s2 ans)
             -> Par det s2 ans
runParVec size comp = 
  runParST (error "runParVec -- this initial value should be unused.") $ do 
    vec <- liftST $ MU.new size
    S.put (UFlp vec)
    comp

-- | A shortcut for the common case of discharging both the vector computation and
-- the underlying `Par` monad at the same time.
runParVec' :: forall elt ans . (MU.Unbox elt) =>
              Int
              -> (forall s1 s2 . ParVec s1 elt 'Det s2 ans)
              -> ans
runParVec' n m = runPar (runParVec n m)

-- | Extract a pointer to the whole Vector in its normal, usable @STVector@ form.
--   Use the `liftST` operator to act on it.
reify :: ParVec s1 elt det s2 (MU.STVector s1 elt)
reify = do
  UFlp vec <- S.get
  return vec

--------------------------------------------------------------------------------

-- | Write to the (implicit) vector state.
write :: Int -> elt -> ParVec s1 elt det s2 ()
write ind val = do
  UFlp vec <- S.get
  liftST$ MU.write vec ind val

-- | Read the (implicit) vector state.
read :: Int -> ParVec s1 elt det s2 elt
read ind = do
  UFlp vec <- S.get
  liftST$ MU.read vec ind 

-- | Return the length of the (implicit) vector state.
length :: MU.Unbox elt => ParVec s1 elt det s2 Int
length = S.get >>= (return . MU.length . unFlp)

-- | Update the vector state by swapping two elements.
swap :: Int -> Int -> ParVec s1 elt det s2 ()
-- swap x y = S.get >>= ((\ v -> MU.swap v x y) . unFlp)
swap x y = do
  UFlp vec <- S.get
  liftST$ MU.swap vec x y

-- | Update the vector state by dropping the first @n@ elements.
drop :: Int -> ParVec s1 elt det s2 ()
drop n = do 
  UFlp vec <- S.get
  S.put (UFlp (MU.drop n vec))

-- | Update the vector state by taking the first @n@ elements, discarding the rest.
take :: Int -> ParVec s1 elt det s2 ()
take n = do 
  UFlp vec <- S.get
  S.put (UFlp (MU.take n vec))

-- | Destructively replace the vector with a bigger vector, adding the given number
-- of elements.  The new elements are uninitialized and will result in errors if
-- read.
grow :: Int -> ParVec s1 elt det s2 ()
grow n = do
  UFlp vec <- S.get
  vec' <- liftST$ MU.grow vec n
  S.put (UFlp vec')

-- | Mutate all the elements of the vector, setting them to the given value.
set :: elt -> ParVec s1 elt det s2 ()
set val = do 
  UFlp vec <- S.get
  liftST $ MU.set vec val

unFlp :: UVectorFlp t t1 -> MU.MVector t1 t
unFlp (UFlp v) = v
