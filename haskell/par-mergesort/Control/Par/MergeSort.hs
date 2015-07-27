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

import Control.Monad.Primitive
import Data.Vector.Generic.Mutable

type Comparison e = e -> e -> Ordering

--------------------------------------------------------------------------------

sort :: (PrimMonad m, MVector v e, Ord e) => v (PrimState m) e -> m ()
sort = undefined

sortBy :: (PrimMonad m, MVector v e) => Comparison e -> v (PrimState m) e -> m ()
sortBy = undefined


--------------------------------------------------------------------------------

sortPar = undefined
