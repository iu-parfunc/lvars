{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RankNTypes      #-}

module Main where

import           Control.Monad (forM_, replicateM)
import           Control.Monad.ST (runST)
-- import qualified Control.Monad.State.Strict   as SS
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as SVM
import           Debug.Trace
import           Data.Int
import           System.Random (randomIO)
import           Control.LVish as LVishSched
import           Control.Par.Class (ParThreadSafe ())
import qualified Control.Par.Class as PC
import           Control.Par.ST
import qualified Control.Par.ST.StorableVec2 as V
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
-- import           Foreign.C.Types (CInt, CLong)
import qualified Control.Par.MergeSort as MS
import           Control.Par.MergeSort.Internal
                  (SSort, SMerge, SSort(..), SMerge(..), findSplit', mergeSort_int32)

main :: IO ()
main = defaultMain $ testGroup "MergeSort tests"
    [ unitTests, properties ]

seqSortMethods :: [SSort]
seqSortMethods  = [CSort, VAMSort, VAISort]

seqMergeMethods :: [SMerge]
seqMergeMethods = [CMerge, HSMerge]


unitTests :: TestTree
unitTests = testGroup "Hand-crafted tests and regression tests"
    [ testCase "Sorting an already sorted vector" $
        testAllVariants 10 10 $ SV.fromList [0 .. 99 :: Int32]
    , testCase "Sorting reversed sorted vector" $
        testAllVariants 10 10 $ SV.fromList [99 .. 0 :: Int32]
    , testCase "Sorting empty vector" $
        testAllVariants 10 10 $ SV.fromList []
    , testCase "REGRESSION: Should work with seq merge threshold = 0" $
        testAllVariants 1 0 $ SV.fromList [1,2,3]
    , testCase "REGRESSION: Sorting singleton vector with thresholds 1" $
        testAllVariants 1 1 $ SV.fromList [0]
    , testCase "REGRESSION: Extracted from QuickCheck generated test, fails with exception" $
        assertBool "" $ checkSorted 4 $ sortPV 0 0 VAMSort HSMerge $
          SV.fromList [0,0,0,0]
    , testCase "REGRESSION: Extracted from QuickCheck generated test, returns wrong" $
        testAllVariants 0 0 $ SV.fromList [0,-1]

    , testCase "Public sort interface / Int32" $
        assertBool "" $ checkSorted 11 $ MS.sort (SV.fromList $ 99:[1..10::Int32])
    -- , testCase "Public sort interface / CInt" $
    --     assertBool "" $ checkSorted 11 $ MS.sort (SV.fromList $ 99:[1..10::CInt])

    -- , testCase "Public sort interface / Int64" $
    --  assertBool "" $ checkSorted 11 $ MS.sort (SV.fromList $ 99:[1..10::Int64])
    -- , testCase "Public sort interface / CLong" $
    --     assertBool "" $ checkSorted 11 $ MS.sort (SV.fromList $ 99:[1..10::CLong])
    -- , testCase "Public sort interface / Int" $
    --     assertBool "" $ checkSorted 11 $ MS.sort (SV.fromList $ 99:[1..10::Int])

    ]
  where
    testAllVariants t1 t2 v =
      forM_ seqSortMethods $ \ssMeth ->
        forM_ seqMergeMethods $ \smMeth -> do
          let msg = "Result not sorted. ssMeth: " ++ show ssMeth
                    ++ " smMeth: " ++ show smMeth
          assertBool msg (checkSorted (SV.length v) $ sortPV t1 t2 ssMeth smMeth v)

findSplitTest :: (SVM.Storable a, Ord a) => [a] -> [a] -> (Int, Int)
findSplitTest l1 l2 = runST $ do
    v1 <- SV.thaw $ SV.fromList l1
    v2 <- SV.thaw $ SV.fromList l2
    findSplit' v1 v2 0 (length l1) 0 (length l2)

properties :: TestTree
properties = testProperty "QuickCheck tests" $ do
    -- TODO(osa): Maybe use an Arbitrary instance, shrink would be useful
    vecSize           <- choose (0, 2 ^ (16 :: Int))
    seqSortThreshold  <- choose (0, vecSize `div` 10)
    seqMergeThreshold <- choose (0, vecSize `div` 10)
    seqSortMethod     <- elements seqSortMethods
    seqMergeMethod    <- elements seqMergeMethods
    lst               <- replicateM vecSize arbitrary
    let vec            = SV.fromList lst
    let ret = sortPV seqSortThreshold seqMergeThreshold
                     seqSortMethod seqMergeMethod vec
    return $ flip counterexample (checkSorted vecSize ret) $ unlines $
               [ "Size: " ++ show vecSize
               , "Seq sort threshold: " ++ show seqSortThreshold
               , "Seq merge threshold: " ++ show seqMergeThreshold
               , "Seq sort method: " ++ show seqSortMethod
               , "Seq merge method: " ++ show seqMergeMethod
               -- , "Vec: " ++ show lst
               ]

-- | Out of place sort on immutable vectors.
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
sortPV' :: (PC.ParMonad p, ParThreadSafe p, PC.ParIVar p, PC.FutContents p (),
            PC.ParFuture p, HasPut e, HasGet e) =>
           Int -> Int -> SSort -> SMerge ->
           SVM.STVector s1 Int32 -> V.ParVec2T s1 Int32 Int32 p e s ()
sortPV' ssThres smThres ssMeth smMeth vec = do
--    (_, right) <- V.reify
--    SS.put (STTup2 (SFlp vec) (SFlp right))
    V.installL vec
    mergeSort_int32 ssThres smThres ssMeth smMeth

mkRandomVec :: Int -> IO (SV.Vector Int32)
mkRandomVec len = SV.generateM len (const randomIO)

checkSorted :: (Show elt, SVM.Storable elt, Ord elt)
            => Int -> SV.Vector elt -> Bool
checkSorted len v
  | len == SV.length v = go 1
  | otherwise = trace ("\n ! Bad length: "++show (SV.length v)) $
                False
  where
    go i
      | i >= SV.length v = True
      | v SV.! (i - 1) <= v SV.! i = go (i + 1)
      | otherwise =
        trace ("\n ! Out of order elements: "++show (v SV.! (i - 1), v SV.! i)) $
              False
