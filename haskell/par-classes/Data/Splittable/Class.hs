{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
{-| 

  A simple type class for data that can be split into pieces for parallel operation,
  and then reassembled.

-}

module Data.Splittable.Class
       (Split(..))
       where

import Control.Applicative
import qualified Data.Foldable as F
import System.IO.Unsafe (unsafePerformIO)

-- | Data that can be split into balanced pieces.  The main application of this is
-- parallel consumption of the data.
class Eq a => Split a where

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



