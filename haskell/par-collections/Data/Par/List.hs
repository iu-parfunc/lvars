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
import qualified Data.Foldable as F

instance Generator [a] where
   type ElemOf [a] = a
   fold  = F.foldl'
   foldM = F.foldlM

