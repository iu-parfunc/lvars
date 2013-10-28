{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

-- | A convenience interface -- simply a restriction of `ParST` to the case
--   of a single, boxed vector as the mutable state.
--
--   This library exposes simple versions of common operations from
--   "Data.Vector.Mutable", which operate directly on the implicit vector state
--   threaded through the monad.

module Control.LVish.ST.Vec
       ( -- * A type alias for parallel computations with @Vector@ state
         ParVec, 
         runParVec, runParVec',

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
import qualified Data.Vector.Mutable as MV
import Prelude hiding (read, length, drop, take)

--------------------------------------------------------------------------------
-- Convenience interface ONE VECTOR state:
--------------------------------------------------------------------------------

type ParVec s1 elt det s2 ans = ParST (MVectorFlp elt s1) det s2 ans

-- | Restricted version of `runParST` which initialized the state with a single,
-- boxed vector of a given size.  All elements start uninitialized.
runParVec :: forall elt s2 det ans .
             Int
             -> (forall s1 . ParVec s1 elt det s2 ans)
             -> Par det s2 ans
runParVec size comp = 
  runParST (error "runParVec -- this initial value should be unused.") $ do 
    vec <- liftST $ MV.new size
    S.put (VFlp vec)
    comp


-- | A shortcut for the common case of discharging both the vector computation and
-- the underlying `Par` monad at the same time.
runParVec' :: forall elt ans .
              Int
              -> (forall s1 s2 . ParVec s1 elt 'Det s2 ans)
              -> ans
runParVec' n m = runPar (runParVec n m)

-- | Extract a pointer to the whole Vector in its normal, usable @STVector@ form.
--   Use the `liftST` operator to act on it.
reify :: ParVec s1 elt det s2 (MV.STVector s1 elt)
reify = do
  VFlp vec <- S.get
  return vec

--------------------------------------------------------------------------------

-- | Write to the (implicit) vector state.
write :: Int -> elt -> ParVec s1 elt det s2 ()
write ind val = do
  VFlp vec <- S.get
  liftST$ MV.write vec ind val

-- | Read the (implicit) vector state.
read :: Int -> ParVec s1 elt det s2 elt
read ind = do
  VFlp vec <- S.get
  liftST$ MV.read vec ind 

-- | Return the length of the (implicit) vector state.
length :: ParVec s1 elt det s2 Int
length = S.get >>= (return . MV.length . unFlp)

-- | Update the vector state by swapping two elements.
swap :: Int -> Int -> ParVec s1 elt det s2 ()
-- swap x y = S.get >>= ((\ v -> MV.swap v x y) . unFlp)
swap x y = do
  VFlp vec <- S.get
  liftST$ MV.swap vec x y

-- | Update the vector state by dropping the first @n@ elements.
drop :: Int -> ParVec s1 elt det s2 ()
drop n = do 
  VFlp vec <- S.get
  S.put (VFlp (MV.drop n vec))

-- | Update the vector state by taking the first @n@ elements, discarding the rest.
take :: Int -> ParVec s1 elt det s2 ()
take n = do 
  VFlp vec <- S.get
  S.put (VFlp (MV.take n vec))


-- | Destructively replace the vector with a bigger vector, adding the given number
-- of elements.  The new elements are uninitialized and will result in errors if
-- read.
grow :: Int -> ParVec s1 elt det s2 ()
grow n = do
  VFlp vec <- S.get
  vec' <- liftST$ MV.grow vec n
  S.put (VFlp vec')

-- | Mutate all the elements of the vector, setting them to the given value.
set :: elt -> ParVec s1 elt det s2 ()
set val = do 
  VFlp vec <- S.get
  liftST $ MV.set vec val

-- Ops not exposed:
----------------------------------------  
-- MV.tail MV.null MV.clear MV.init MV.slice 

-- Here are ops that don't directly make sense with a single, global vector.
----------------------------------------  
--    MV.copy MV.clone MV.splitAt MV.move  MV.unsafeInit  MV.overlaps
    
-- Vector creation:
--    MV.replicate    MV.new MV.replicateM

-- And here are the unsafe ops:
----------------------------------------
--    MV.unsafeDrop MV.unsafeRead MV.unsafeWrite  
--  MV.unsafeGrow MV.unsafeSlice MV.unsafeSwap
-- MV.unsafeMove MV.unsafeTail
-- MV.unsafeCopy MV.unsafeNew MV.unsafeTake  
unFlp :: MVectorFlp t t1 -> MV.MVector t1 t
unFlp (VFlp v) = v
