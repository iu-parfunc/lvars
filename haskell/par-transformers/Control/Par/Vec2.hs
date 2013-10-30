{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

-- | A convenience interface -- simply a restriction of `ParST` to the case
--   of a single, boxed vector as the mutable state.
--
--   This library exposes simple versions of common operations from
--   "Data.Vector.Mutable", which operate directly on the implicit vector state
--   threaded through the monad.

module Control.LVish.ST.Vec2
       ( -- * A type alias for parallel computations with @Vector@ state
         ParVec2, 
         runParVec2, runParVec2',

         -- * Reexported from the generic interface
         forkSTSplit, liftPar, liftST, 
         
         -- * Retrieving an explict pointer to the Vector
         reify, liftST,
       )
       where

import Control.LVish (Par, Determinism(Det), runPar)
import Control.LVish.ST
import qualified Control.Monad.State.Strict as S
import qualified Data.Vector.Mutable as MV
import Prelude hiding (read, length, drop, take)

--------------------------------------------------------------------------------

type ParVec2 s1 elt1 elt2 det s2 ans =
     ParST (STTup2 (MVectorFlp elt1) (MVectorFlp elt2) s1) det s2 ans

-- | Restricted version of `runParST` which initialized the state with a single,
-- boxed vector of a given size.  All elements start uninitialized.
runParVec2 :: forall elt1 elt2 s2 det ans .
             (Int,Int)
             -> (forall s1 . ParVec2 s1 elt1 elt2 det s2 ans)
             -> Par det s2 ans
runParVec2 (size1,size2) comp = 
  runParST (error "runParVec -- this initial value should be unused.") $ do 
    vec1 <- liftST $ MV.new size1
    vec2 <- liftST $ MV.new size2
    S.put (STTup2 (VFlp vec1) (VFlp vec2))
    comp

-- | A shortcut for the common case of discharging both the vector computation and
-- the underlying `Par` monad at the same time.
runParVec2' :: forall elt1 elt2 ans .
              (Int,Int)
              -> (forall s1 s2 . ParVec2 s1 elt1 elt2 'Det s2 ans)
              -> ans
runParVec2' pr m = runPar (runParVec2 pr m)

-- | Extract a pointer to the whole Vector in its normal, usable @STVector@ form.
--   Use the `liftST` operator to act on it.
reify :: ParVec2 s1 elt1 elt2 det s2 (MV.STVector s1 elt1, MV.STVector s1 elt2)
reify = do
  (STTup2 (VFlp vec1) (VFlp vec2)) <- S.get
  return (vec1,vec2)

