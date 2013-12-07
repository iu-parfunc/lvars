{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

-- | Provide instances for parallel handling of common, pure Haskell data structures.

module Data.Par.Instances
       () where

import Data.Splittable.Class (Split(..))
import qualified Control.Par.Class as PC
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Foldable as F

import qualified Data.Par.Splittable as Sp 

import Control.Applicative
import Data.Monoid

--------------------------------------------------------------------------------

#ifdef NEWCONTAINERS

instance Eq a => Split (S.Set a) where
  {-# INLINE split #-}
  split = S.splitRoot

instance (Eq k, Eq v) => Split (M.Map k v) where
  {-# INLINE split #-}
  split = M.splitRoot

instance PC.Generator (M.Map k v) where
  type ElemOf (M.Map k v) = (k,v)
  {-# INLINE foldM #-}  
  foldM = foldrMWithKey
  {-# INLINE fold #-}
  fold fn = M.foldlWithKey (\ !a k v -> fn a (k,v)) 

instance PC.Generator (S.Set a) where
  type ElemOf (S.Set a) = a
  {-# INLINE foldM #-}  
  foldM = F.foldlM
  {-# INLINE fold #-}  
  fold  = F.foldl'
  
-- TODO: Opt in to the trivial instance of ParFoldable:

-- instance PC.ParFoldable (M.Map k v) where
--  pmapFold = Sp.pmapReduce
  
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
