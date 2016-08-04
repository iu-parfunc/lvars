{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Main where

import           Data.Int
import qualified Data.Vector.Storable           as SV
import qualified Data.Vector.Storable.Mutable   as SVM

import           Criterion
import           Criterion.Main

import           Control.LVish                  as LVishSched
import qualified Control.Par.Class              as PC
import           Control.Par.MergeSort.Internal
import           Control.Par.ST
import qualified Control.Par.ST.StorableVec2    as V

-- TODO: Add vector-algortihms benchmarks.

main :: IO ()
main = defaultMain
  [ env (return $ SV.fromList $ reverse [0 .. 10^(6 :: Int32)]) $ \(vec :: SV.Vector Int32) ->
      bgroup "sorting benchmarks" $ concat $
        flip map [CSort, VAMSort, VAISort] $ \ssMeth ->
          flip map [CMerge, HSMerge] $ \smMeth ->
            -- TODO(osa): Thresholds should be varying?
            mkBench 100 100 ssMeth smMeth vec
  ]

mkBench :: Int -> Int -> SSort -> SMerge -> SV.Vector Int32 -> Benchmark
mkBench ssThres smThres ssMeth smMeth vec =
    bench msg $ nf (sortPV ssThres smThres ssMeth smMeth) vec
  where
    msg = "sort" ++ " " ++ show ssThres
                 ++ " " ++ show smThres
                 ++ " " ++ show ssMeth
                 ++ " " ++ show smMeth

-- | Out of place sort on immutable vectors.
--
-- TODO(osa): Copied from tests, maybe copy to a shared place.
--
sortPV :: Int -> Int -> SSort -> SMerge -> SV.Vector Int32 -> SV.Vector Int32
sortPV ssThres smThres ssMeth smMeth vec =
    -- TODO(osa): Maybe remove copying here by just taking mutable vec and
    -- returning mutable one.
    LVishSched.runPar $ V.runParVec2T (0, SV.length vec) $ do
      vec' <- liftST $ SV.thaw vec
      sortPV' ssThres smThres ssMeth smMeth vec' >> do
        (rawL, _) <- V.reify
        sv <- liftST $ SV.freeze rawL
        return $ sv

-- | Sort the vector in the left component of the state.
--
-- TODO(osa): Like sortPV, copied.
sortPV' :: (PC.ParMonad p, PC.ParThreadSafe p, PC.ParIVar p, PC.FutContents p (),
            PC.ParFuture p, HasPut e, HasGet e) =>
           Int -> Int -> SSort -> SMerge ->
           SVM.STVector s1 Int32 -> V.ParVec2T s1 Int32 Int32 p e s ()
sortPV' ssThres smThres ssMeth smMeth vec = do
--    (_, right) <- V.reify
--    SS.put (STTup2 (SFlp vec) (SFlp right))
    V.installL vec
    mergeSort_int32 ssThres smThres ssMeth smMeth
