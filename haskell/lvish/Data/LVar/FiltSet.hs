{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs  #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.LVar.FiltSet
       {-(
         SaturatingLVar(..)
       , FiltSet(..)
       , AFoldableOrd(..)
       , newEmptySet
       , newFromList
       , insert
       , fromFiltSet
       )-}
       where

import GHC.Prim

import Control.LVish
import Control.LVish.DeepFrz.Internal
import Control.LVish.Internal
import Internal.Control.LVish.SchedIdempotent (newLV, putLV)

import Control.Monad (forM)
import Data.Atomics (atomicModifyIORefCAS)
import qualified Data.Foldable as F
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import Data.IORef
-- import Data.TLS.GCC
import Data.TLS.GHC
import GHC.Prim (unsafeCoerce#)
import System.IO.Unsafe (unsafeDupablePerformIO)

newtype FiltSet s f a = FiltSet (LVar s (TLS (IORef ([f s a], [f s a]))) (f s a))

data AFoldableOrd a = forall f . (F.Foldable f, Ord (f a)) => AFoldableOrd (f a)

-- data AFoldableOrd2 f a = forall f2 . (F.Foldable f2, Ord (f2 a)) => AFoldableOrd2 (f2 a)
-- data AFoldable a = forall f . (F.Foldable f) => AFoldable (f a)
-- data SetOfFoldable a = forall f . (F.Foldable f, Ord (f a)) => SetOfFoldable (S.Set (f a))

{- -- OPTION 1: 
class SaturatingLVar f where
  type Normalized f :: * -> *
    
  -- | Drive the variable to top.  This is equivalent to an insert of a
  -- conflicting binding.
  saturate :: f s a -> Par e s ()
  -- | Register a callback that is only called if the SatMap LVar
  -- becomes /saturated/.
  whenSat :: f s a -> Par e s () -> Par e s ()
  -- | Is the variable saturated?
  isSat :: f Frzn a -> Bool
  isSat = unsafeIsSat
  unsafeIsSat :: f s a -> Bool
  finalizeOrd :: Ord a => f Frzn a -> Maybe (AFoldableOrd a)
                 
  prop1 :: Proxy f -> forall a . ReifyDict (Ord (Normalized f a))                 
  finalize :: f Frzn a -> Maybe ((Normalized f) a)

data ReifyDict c where 
  MkDict :: c => ReifyDict c
-}

-- OPTION 2               
class Ord (Normalized f s a) => SaturatingLVar f s a where
  type Normalized f s a :: * 

  -- | Drive the variable to top.  This is equivalent to an insert of a
  -- conflicting binding.
  saturate :: f s a -> Par e s ()
  -- | Register a callback that is only called if the SatMap LVar
  -- becomes /saturated/.
  whenSat :: f s a -> Par e s () -> Par e s ()

  -- | Is the variable saturated?
--  isSat :: f Frzn a -> Bool
--  isSat = unsafeIsSat

  unsafeIsSat :: forall s0 . f s0 a -> Bool
  finalizeOrd :: Ord a => f Frzn a -> Maybe (AFoldableOrd a)

  finalize :: f Frzn a -> Maybe (Normalized f s a)


newEmptySet :: SaturatingLVar f s a => Par e s (FiltSet s f a)
newEmptySet = newFromList []

newFromList :: SaturatingLVar f s a => [f s a] -> Par e s (FiltSet s f a)
newFromList l = WrapPar $ fmap (FiltSet . WrapLVar) $ newLV $ vec
  where vec = do
          tls <- mkTLS (newIORef ([], []))
          local <- getTLS tls
          writeIORef local (l, [])
          return tls

insert :: (HasPut e, SaturatingLVar f s a) => f s a -> FiltSet s f a -> Par e s ()
insert !elm !fs@(FiltSet (WrapLVar lv)) = let
  insert' e (x:y:right, left) = ((e:right, filter notSat [x, y] ++ left), Just e)
  insert' e (right, left) = ((e:(filter notSat right ++ left), []), Just e)
  notSat = not . unsafeIsSat
  putter tls = do
          ref <- getTLS tls
          atomicModifyIORefCAS ref (insert' elm)
  in WrapPar (putLV lv putter)

fromFiltSet :: (Ord a, Ord (AFoldableOrd a), SaturatingLVar lv Frzn a) =>
               FiltSet Frzn lv a -> S.Set (AFoldableOrd a)
fromFiltSet (FiltSet lv) = unsafeDupablePerformIO $ do
                             lvs <- allTLS (state lv) >>= mapM readIORef
                             return $ S.fromList $ catMaybes $ map finalizeOrd $ conc lvs
                             where
                               conc = F.foldl' (\acc (r, l) -> (r ++ l)++acc) []

instance DeepFrz a => DeepFrz (FiltSet s f a) where
  type FrzType (FiltSet s f a) = FiltSet Frzn f (FrzType a)
  frz = unsafeCoerce#
