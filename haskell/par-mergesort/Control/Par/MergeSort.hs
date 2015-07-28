{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE ScopedTypeVariables  #-}

-- | A fast, parallel mergesort.
--
--   This module exposes good default configurations, with simple interfaces.

module Control.Par.MergeSort
       (
       -- * Simple sorts

       -- | These are drop-in replacements for their counterparts in `vector-algorithms`
       sort, sortBy, Comparison,

       -- * Par-monad sorts
       sortPar

       -- * Unboxed and storable sorts

       )
 where

import           Control.Monad.Primitive
import           Data.Vector.Generic.Mutable
import           Data.Vector.Generic as G
import qualified Data.Vector.Mutable as MV
-- import qualified Control.Par.ST as PST
-- import qualified Control.Par.ST.Vec2 as V2
import qualified Data.Vector.Storable.Mutable as SV
import qualified Data.Vector.Storable as S
import qualified Control.Par.ST.StorableVec2 as S2
import           Control.Par.MergeSort.Internal
-- import        Data.Vector.Par.MergeSort
import           Control.Par.Class (ParThreadSafe ())
import qualified Control.Par.Class as PC
import           Control.Par.EffectSigs
import qualified Control.LVish as LV


--------------------------------------------------------------------------------

type Comparison e = e -> e -> Ordering


-- | Perform an out-of-place sort on a pure vector.
--
sort :: (S.Storable e, Ord e) => S.Vector e -> S.Vector e
sort vec = LV.runPar (sortPar vec)

-- -- The type is generic, but this should only be used for BOXED
-- -- vectors.  Unboxed/Storable vectors have more efficient
-- -- counterparts.
-- sort :: (G.Vector v e, Ord e) => v e -> v e
-- sort = undefined

-- | In-place version of sort that mutates the input vector.
sort' :: (PrimMonad m, MVector v e, Ord e) => v (PrimState m) e -> m ()
sort' = undefined

-- TODO: check if this can produce nondeterministic outcomes given an
-- adversarial (wrong) comparison function.
sortBy :: (PrimMonad m, MVector v e) => Comparison e -> v (PrimState m) e -> m ()
sortBy = undefined

--   LV.runPar $ V.runParVec2T (0,size) $

--------------------------------------------------------------------------------



sortPar :: forall p e elt s .
           (ParThreadSafe p, PC.FutContents p (), PC.ParIVar p,
            PC.ParFuture p, HasGet e, HasPut e,
            Ord elt, SV.Storable elt)
        => S.Vector elt
        -> p e s (S.Vector elt)
sortPar vec =
  -- Allocate the temporary buffer.  But null-out the left side which
  -- we'll replace in a moment:
  S2.runParVec2T (0, S.length vec) comp
  where
   comp :: S2.ParVec2T s1 elt elt p e s (S.Vector elt)
   comp = do vec' <- S2.liftST (S.thaw vec)
             S2.installL vec'
--             mergeSort 2048 2048 CSort CMerge    -- Breaks in ghci.
--             mergeSort 2048 2048 VAMSort HSMerge -- Works
--             mergeSort 2048 2048 VAMSort CMerge  -- Works
             mergeSort 2048 2048 CSort HSMerge    -- Breaks in ghci.
             (left,_) <- S2.reify
             S2.liftST $ S.freeze left
