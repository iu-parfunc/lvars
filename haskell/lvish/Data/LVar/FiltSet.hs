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
import Control.LVish.DeepFrz (runParThenFreeze)
import Control.LVish.DeepFrz.Internal
import Control.LVish.Internal
import Data.LVar.Generic(PartialJoinSemiLattice(..))
import Data.LVar.LayeredSatMap (LayeredSatMap)
import Data.LVar.LayeredSatMap (LayeredSatMap(..), LSMContents(..))
import qualified Data.LVar.LayeredSatMap as LSM
import qualified Control.LVish.Internal.SchedIdempotent as L

import Control.Monad (forM)
import Data.Atomics (atomicModifyIORefCAS)
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Maybe (catMaybes, isJust)
import Data.Monoid (Monoid)
import qualified Data.Set as S
import Data.IORef
-- import Data.TLS.GCC
import Data.TLS.GHC
import GHC.Prim (unsafeCoerce#)
import System.IO.Unsafe (unsafeDupablePerformIO)

newtype FiltSet s f a = FiltSet (LVar s (TLS (IORef ([f s a], [f s a]))) (f s a))

data AFoldableOrd a = forall f . (F.Foldable f, Ord (f a)) => AFoldableOrd (f a)

class SaturatingLVar f where
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

instance Ord k => SaturatingLVar (LayeredSatMap k) where
--  type Finalizer (LayeredSatMap k) v = PartialJoinSemiLattice v
  whenSat (LayeredSatMap lv) (WrapPar newact) = WrapPar $ do
      L.logStrLn 5 " [LayeredSatMap] whenSat issuing atomicModifyIORef on state"
      let LSMContents (mpRef:mps) = state lv
      x <- L.liftIO $ atomicModifyIORef' mpRef fn
      case x of
        Nothing -> L.logStrLn 5 " [LayeredSatMap] whenSat: not yet saturated, registered only."
        Just a -> L.logStrLn 5 " [LayeredSatMap] whenSat invoking saturation callback" >> a
      where
        fn n@(Nothing, _) = (n, Just newact)
        fn (Just m, onsat) = let onsat' = onsat >> newact in
          ((Just m, onsat'), Nothing)

  saturate (LayeredSatMap lv) = WrapPar $ do
      let lvid = L.lvarDbgName $ unWrapLVar lv
          LSMContents (mpRef:mps) = state lv
      L.logStrLn 5 $ " [LayeredSatMap] saturate: explicity saturating lvar " ++ lvid
      act <- L.liftIO $ atomicModifyIORef' mpRef fn
      case act of
        Nothing -> L.logStrLn 5 $ " [LayeredSatMap] saturate: done saturating lvar " ++ lvid ++ ", no callbacks to invoke."
        Just a -> L.logStrLn 5 (" [SatMap] saturate: done saturating lvar " ++ lvid ++ ".  Now invoking callback.") >> a
      where
        fn n@(Nothing, _) = (n, Nothing)
        -- Should this be returning (Nothing, return ()) or something? We don't need the callback anymore
        fn (Just m, onsat) = ((Nothing, onsat), Just onsat)

  unsafeIsSat (LayeredSatMap lv) = unsafeDupablePerformIO $ do
    let LSMContents (mpRef:mps) = state lv
    (mp, _) <- readIORef mpRef
    return $ not $ isJust mp

  finalizeOrd lsm@(LayeredSatMap lv) = case LSM.fromIMap lsm of
                                         Nothing -> Nothing
                                         Just m -> Just $ AFoldableOrd m

newEmptySet :: Par e s (FiltSet s f a)
newEmptySet = newFromList []

newFromList :: [f s a] -> Par e s (FiltSet s f a)
newFromList l = WrapPar $ fmap (FiltSet . WrapLVar) $ L.newLV $ vec
  where vec = do
          tls <- mkTLS (newIORef ([], []))
          local <- getTLS tls
          writeIORef local (l, [])
          return tls

insert :: SaturatingLVar f => f s a -> FiltSet s f a -> Par e s ()
insert !elm !fs@(FiltSet (WrapLVar lv)) = let
  insert' e (x:y:right, left) = ((e:right, filter notSat [x, y] ++ left), Just e)
  insert' e (right, left) = ((e:(filter notSat right ++ left), []), Just e)
  notSat = not . unsafeIsSat
  putter tls = do
          ref <- getTLS tls
          atomicModifyIORefCAS ref (insert' elm)
  in WrapPar (L.putLV lv putter)

fromFiltSet :: (PartialJoinSemiLattice v, Ord v, Ord k) =>
               FiltSet Frzn (LayeredSatMap k) v -> S.Set (M.Map k v)
fromFiltSet (FiltSet lv) = unsafeDupablePerformIO $ do
                             lvs <- allTLS (state lv) >>= mapM readIORef
                             let conc = F.foldl' (\acc (r, l) -> (r ++ l)++acc) [] lvs
                                 fldbls = catMaybes $ map LSM.fromIMap conc
                             return $ S.fromList fldbls

instance DeepFrz a => DeepFrz (FiltSet s f a) where
  type FrzType (FiltSet s f a) = FiltSet Frzn f (FrzType a)
  frz = unsafeCoerce#

test1 :: FiltSet Frzn (LayeredSatMap String) Int
test1 = runParThenFreeze $ isDet $ do
  m <- LSM.newMap $ M.fromList [("x", 0 :: Int)]
  m' <- LSM.pushLayer m
  m'' <- LSM.pushLayer m
  LSM.insert "x" 2 m
  LSM.insert "x" 4 m'
  LSM.insert "y" 0 m'
  LSM.insert "x" 5 m''
  newFromList [m, m', m'']
