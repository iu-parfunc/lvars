{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}

-- | Provide instances for parallel handling of common, pure Haskell data structures.

module Data.Par.Map
       () where

import qualified Control.Par.Class     as PC
import qualified Data.Map              as M
import           Data.Splittable.Class (Split (..))

-- import Control.Applicative
-- import           Data.Monoid

--------------------------------------------------------------------------------

instance PC.Generator (M.Map k v) where
  type ElemOf (M.Map k v) = (k,v)
  {-# INLINE foldM #-}
  foldM = foldrMWithKey
  {-# INLINE fold #-}
  fold fn = M.foldlWithKey (\ !a k v -> fn a (k,v))

#ifdef NEWCONTAINERS
instance (Eq k, Eq v) => Split (M.Map k v) where
  {-# INLINE split #-}
  split = M.splitRoot

-- TODO: Opt in to the trivial instance of ParFoldable, using Split-based mapreduce:
-- instance PC.ParFoldable (M.Map k v) where
--  pmapFold = Sp.pmapReduce
#else
-- instance PC.ParFoldable (M.Map k v) where
#endif

foldrMWithKey :: Monad m => (acc -> (k, v) -> m acc) -> acc -> M.Map k v -> m acc
foldrMWithKey fn zer mp =
   M.foldrWithKey (\ k v m -> m >>= fn2 (k,v)) (return zer) mp
  where
    fn2 !pr !a = fn a pr
{-# INLINE foldrMWithKey #-}

{-

foldrMWithKey :: Monad m => ((k, v) -> acc -> m acc) -> acc -> M.Map k v -> m acc
-- foldrMWithKey :: Applicative m => ((k, v) -> acc -> m acc) -> acc -> M.Map k v -> m acc
foldrMWithKey fn zer mp =
  undefined
--  M.foldMapWithKey (\ k v -> undefined) mp

newtype FoldAction_ f acc = FoldAction_ { runFoldAction_ :: acc -> f acc }
instance Applicative f => Monoid (FoldAction_ f acc) where
  mempty = FoldAction_ (\ x -> pure x)
  FoldAction_ a `mappend` FoldAction_ b = FoldAction_ (a *> b)

-}
