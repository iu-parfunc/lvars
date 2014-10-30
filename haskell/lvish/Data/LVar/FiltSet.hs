{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.LVar.FiltSet
       (
         SaturatingLVar(..)
       , FiltSet(..)
       , AFoldableOrd(..)
       , newEmptySet
       , newFromList
       , insert
       , fromFiltSet
       )
       where

import GHC.Prim

import Control.LVish
import Control.LVish.DeepFrz.Internal
import Control.LVish.Internal
import Internal.Control.LVish.SchedIdempotent (newLV, putLV)

import Data.Atomics (atomicModifyIORefCAS)
import qualified Data.Foldable as F
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import Data.IORef
import GHC.Prim (unsafeCoerce#)
import System.IO.Unsafe (unsafeDupablePerformIO)

newtype FiltSet s f a = FiltSet (LVar s (IORef [f s a]) (f s a))

data AFoldableOrd a = forall f . (F.Foldable f, Ord (f a)) => AFoldableOrd (f a)

class SaturatingLVar f where
  -- type Finalizer f v :: Constraint
  -- type Finalizer f v = ()
  -- | Drive the variable to top.  This is equivalent to an insert of a
  -- conflicting binding.
  saturate :: f s a -> Par e s ()
  -- | Register a callback that is only called if the SatMap LVar
  -- becomes /saturated/.
  whenSat :: f s a -> Par e s () -> Par e s ()
  -- | Is the variable saturated?
  isSat :: f Frzn a -> Bool
  finalizeOrd :: Ord a => f Frzn a -> Maybe (AFoldableOrd a)

newEmptySet :: SaturatingLVar f => Par e s (FiltSet s f a)
newEmptySet = newFromList []

newFromList :: SaturatingLVar f => [f s a] -> Par e s (FiltSet s f a)
newFromList l = WrapPar $ fmap (FiltSet . WrapLVar) $ newLV $ newIORef l

insert :: (HasPut e, SaturatingLVar f) => f s a -> FiltSet s f a -> Par e s ()
insert !elm !fs@(FiltSet (WrapLVar lv)) = WrapPar (putLV lv putter)
  where putter ref = atomicModifyIORefCAS ref update
        update l = (elm:l, Just elm)

fromFiltSet :: (Ord a, Ord (AFoldableOrd a), SaturatingLVar lv) => FiltSet Frzn lv a -> S.Set (AFoldableOrd a)
fromFiltSet (FiltSet lv) = unsafeDupablePerformIO $ do
                             lvs <- readIORef $ state lv
                             return $ S.fromList $ catMaybes $ map finalizeOrd lvs
-- fromFiltSet :: (SaturatingLVar f, Ord (f Frzn a)) => FiltSet Frzn f a -> S.Set (f Frzn a)
-- fromFiltSet (FiltSet lv) = unsafeDupablePerformIO $ do
--   lvs <- readIORef $ state lv
--   return . S.fromList $ filter (not . isSat) lvs

instance DeepFrz a => DeepFrz (FiltSet s f a) where
  type FrzType (FiltSet s f a) = FiltSet Frzn f (FrzType a)
  frz = unsafeCoerce#
