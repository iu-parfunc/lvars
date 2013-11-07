

{-| 

  A simple type class for data that can be split into pieces for parallel operation,
  and then reassembled.

-}

module Data.Splittable (Split(..)) where


class Eq a => Split a where
  split   :: a -> [a]
  combine :: [a] -> a
  empty   :: a 
--  split2  :: a -> (a)
--  split3  :: a -> (a,a,a)
