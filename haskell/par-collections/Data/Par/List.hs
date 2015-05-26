{-# LANGUAGE TypeFamilies #-}

-- | Lists are NOT good data structures for parallel programming.  Nevertheless, you
--   may find yourself with a list you need to consume in parallel.  This library
--   provides various ways to do that, but they are all less efficient than using a
--   balanced, splittable datatype to begin with.

module Data.Par.List
       (
       )
       where

import Control.Par.Class
import Data.Splittable.Class (Split(..))
import qualified Data.Foldable as F
import qualified Data.Par.Splittable as Sp 
import Data.List as L

instance Generator [a] where
   type ElemOf [a] = a
   fold  = F.foldl'
   foldM = F.foldlM

-- | WARNING: this instance is inefficient, because lists are NOT good
-- splittable structures.  Nevertheless, lists are ubiquitous, so it's
-- better to have this than not.
instance Split [a] where
  {-# INLINABLE split #-}
  split ls =
    let len = length ls 
        (l,r) = L.splitAt (len `quot` 2) ls
    in [l,r]

-- TODO: If we're ok with depending on the "split" package, then we can write a good splitPlease:
--  splitPlease n ls = 
    
----------------------------------------
  -- NOTE: if there is no guarantee on ordering, then it might make
  -- sense to split into the even and odd elements.  That layer on
  -- thunks, but will not require traversing to the end of the list
  -- for the result of split to get to WHNF.
----------------------------------------
