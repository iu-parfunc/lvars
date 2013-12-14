{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

-- | Instances of `Data.Splittable.Split` for vector datatypes.

module Data.Par.Vector () where

-- import qualified Data.Vector as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM

import Control.Monad.Primitive (PrimMonad, PrimState)

import Data.Splittable.Class (Split(..))

-- M.read
--   :: (Control.Monad.Primitive.PrimMonad m, MVector v a) =>
--      v (Control.Monad.Primitive.PrimState m) a -> Int -> m a


--instance (PrimMonad m, GM.MVector v a) => Split (v (PrimState m) a) where
instance (GM.MVector v a, Eq (v s a)) => Split (v s a) where
  split vec =
    case GM.length vec of
      0 -> []
      1 -> [vec]
      n -> let [GM.slice 0 
