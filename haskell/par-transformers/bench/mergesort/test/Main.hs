{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import           Data.Int                     (Int32)

import qualified Control.Monad.State.Strict   as SS
import qualified Data.Vector.Storable         as SV
import qualified Data.Vector.Storable.Mutable as SVM
import           System.Random                (randomIO)

import           Control.LVish                as LVishSched
import           Control.Par.Class            (ParThreadSafe ())
import qualified Control.Par.Class            as PC
import           Control.Par.ST
import qualified Control.Par.ST.StorableVec2  as V

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Control.Par.MergeSort

main :: IO ()
main = defaultMain $ testProperty "Correctness tests" $ do
    vecSize           <- choose (0, 2 ^ (16 :: Int))
    seqSortThreshold  <- choose (0, vecSize `div` 10)
    seqMergeThreshold <- choose (0, vecSize `div` 10)
    seqSortMethod     <- elements [CSort, VAMSort, VAISort]
    seqMergeMethod    <- elements [CMerge, TMerge, MPMerge]
    vec               <- SV.fromList <$> arbitrary
    let ret = sortPV seqSortThreshold seqMergeThreshold
                     seqSortMethod seqMergeMethod vec
        test = flip counterexample (checkSorted ret) $ unlines $
                 [ "Size: " ++ show vecSize
                 , "Seq sort threshold: " ++ show seqSortThreshold
                 , "Seq merge threshold: " ++ show seqMergeThreshold
                 , "Seq sort method: " ++ show seqSortMethod
                 , "Seq merge method: " ++ show seqMergeMethod
                 ]
    return test

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

sortPV' :: (PC.ParMonad p, ParThreadSafe p, PC.ParIVar p, PC.FutContents p (),
            PC.ParFuture p, HasPut e, HasGet e) =>
           Int -> Int -> SSort -> SMerge ->
           SVM.STVector s1 Int32 -> V.ParVec2T s1 Int32 Int32 p e s ()
sortPV' ssThres smThres ssMeth smMeth vec = do
    STTup2 _ (SFlp right) <- SS.get
    SS.put (STTup2 (SFlp vec) (SFlp right))
    mergeSort ssThres smThres ssMeth smMeth

mkRandomVec :: Int -> IO (SV.Vector Int32)
mkRandomVec len = SV.generateM len (const randomIO)

checkSorted :: SV.Vector Int32 -> Bool
checkSorted v = go 1
  where
    go i
      | i >= SV.length v = True
      | otherwise        = (v SV.! (i - 1) <= v SV.! i) && go (i + 1)
