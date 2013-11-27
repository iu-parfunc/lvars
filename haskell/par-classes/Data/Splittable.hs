

{-| 

  A simple type class for data that can be split into pieces for parallel operation,
  and then reassembled.

-}

module Data.Splittable (Split(..)) where

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

--  split2  :: a -> (a,a)
--  split3  :: a -> (a,a,a)



-- class Generator c e | c -> e where 
