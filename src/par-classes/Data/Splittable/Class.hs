{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-|

  A simple type class for data that can be split into pieces for parallel operation,
  and then reassembled.

-}

module Data.Splittable.Class
       (Split(..))
       where

import qualified Data.List as L

-- | Data that can be split into balanced pieces.  The main application of this is
-- parallel consumption of the data.
class Split a where
-- class Eq a => Split a where

  -- | Split the data value into pieces.  An empty data structure may return an empty
  -- list.
  split :: a -> [a]

  -- | A variant of `split` that allows the user to provide a /hint/ as to how many
  -- pieces they would like to split into.  There is no obligation for the
  -- implementation to follow this hint (either as an upper or lower bound).
  splitPlease :: Int -> a -> [a]
  -- The defaul implementation ignorse the hint:
  splitPlease _ = split

  -- -- | The inverse of split.
  -- combine :: [a] -> a

--  empty   :: a

-- In some cases we may know exactly how many pieces the underlying data structur
-- can produce efficiently.

--  split2  :: a -> (a,a)
--  split3  :: a -> (a,a,a)

-- | WARNING: this instance is inefficient, because lists are NOT good
-- splittable structures.  Nevertheless, lists are ubiquitous, so it's
-- better to have this than not.
instance Split [a] where
  {-# INLINABLE split #-}
  split ls =
    let len = length ls
        (l,r) = L.splitAt (len `quot` 2) ls
    in [l,r]
