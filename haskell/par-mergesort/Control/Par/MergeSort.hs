{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DoAndIfThenElse            #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Control.Par.MergeSort where

import           Control.LVish                as LVishSched
-- TODO(osa): Disabling these until we implement instances:
-- import qualified Control.Monad.Par.Scheds.Direct as DirectSched
-- import qualified Control.Monad.Par.Scheds.Sparks as SparksSched
-- import qualified Control.Monad.Par.Scheds.Trace  as TraceSched

import           Control.Par.Class            (ParThreadSafe ())
import qualified Control.Par.Class            as PC
import           Control.Par.Class.Unsafe     (internalLiftIO)
import           Control.Par.ST

import           Control.Monad
import           Control.Monad.ST             (ST)
import qualified Control.Monad.State.Strict   as SS

-- TODO(osa): Removed all variants to make test program working, maybe move each
-- specialzed version to it's own module.

#define VFlp SFlp
#define MVectorFlp SVectorFlp
import qualified Control.Par.ST.StorableVec2  as V
import qualified Data.Vector.Storable.Mutable as MV

-- [2013.11.15] Adding new variant:
import qualified Data.Vector.Algorithms.Intro as VI
import qualified Data.Vector.Algorithms.Merge as VA

-- FFI stuff for Cilk parts
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Ptr

data SMerge = CMerge | MPMerge
  deriving (Show, Read)

data SSort = CSort | VAMSort | VAISort
  deriving (Show, Read)

data ParSort = InPlace | OutPlace
  deriving (Show, Read)

-- | Given a vector in left position, and an available buffer of equal
-- size in the right position, sort the left vector.
{-# INLINE mergeSort #-}
mergeSort :: (ParThreadSafe p, PC.FutContents p (), PC.ParIVar p,
              HasGet e, HasPut e,
              PC.ParFuture p, Ord e1, Show e1) =>
             Int -> Int -> SSort -> SMerge ->
             V.ParVec2T s1 e1 e1 p e s ()
mergeSort !st !mt !sa !ma = do
  len <- V.lengthL
  unless (len <= 1) $
    if len < st then do
      case sa of
        VAMSort -> seqSortL
        VAISort -> seqSortL2
        CSort   -> cilkSeqSort
    else do
      let sp = len `quot` 2
      void $ forkSTSplit (sp,sp)
        (do void $ forkSTSplit (sp,sp)
              (mergeSort st mt sa ma)
              (mergeSort st mt sa ma)
            mergeTo2 sp mt ma)
        (do void $ forkSTSplit (sp,sp)
              (mergeSort st mt sa ma)
              (mergeSort st mt sa ma)
            mergeTo2 sp mt ma)
      mergeTo1 sp mt ma

-- mergeSortOutPlace ::
--             (ParThreadSafe p, PC.FutContents p (),
--               PC.ParFuture p, Ord e1, Show e1) =>
--              Int -> Int -> SSort -> SMerge ->
--              V.ParVec2T s1 e1 e1 p e s ()
-- mergeSortOutPlace !st !mt !sa !ma = do
--   error "mergeOutPlace"
{-
 where
  cpuMergeSort t cpuMS vec =
    if V.length vec <= t
    then cpuMS vec
    else do
      let n = (V.length vec) `div` 2
      let (lhalf, rhalf) = V.splitAt n vec
      ileft <- spawn_ (cpuMergeSort t cpuMS lhalf)
      right <-         cpuMergeSort t cpuMS rhalf
      left  <- get ileft
      merge t left right
-}

-- | Call a sequential in-place sort on the left vector.
{-# INLINE seqSortL #-}
seqSortL :: (Ord eL, ParThreadSafe p) => V.ParVec2T s1 eL eR p e s ()
seqSortL = do
  STTup2 (VFlp vecL) (VFlp _) <- SS.get
  liftST $ VA.sort vecL

{-# INLINE seqSortL2 #-}
seqSortL2 :: (Ord eL, ParThreadSafe p) => V.ParVec2T s1 eL eR p e s ()
seqSortL2 = do
  STTup2 (VFlp vecL) (VFlp _) <- SS.get
  liftST $ VI.sort vecL

---------------

-- | Outer wrapper for a function that merges input vectors in the
-- left position into the vector in right position.
{-# INLINE mergeTo2 #-}
mergeTo2 :: (ParThreadSafe p, Ord e1, Show e1, PC.FutContents p (),
             PC.ParFuture p, HasGet e, HasPut e, PC.ParIVar p) =>
            Int -> Int -> SMerge -> V.ParVec2T s1 e1 e1 p e s ()
mergeTo2 sp threshold ma = do
  -- convert the state from (Vec, Vec) to ((Vec, Vec), Vec) then call normal parallel merge
  transmute (morphToVec21 sp) (pMergeTo2 threshold ma)
  -- error "mergeTo2"


-- | Type alias for a ParST state of ((Vec,Vec), Vec)
type ParVec21T s1 e1 p e s a =
     ParST (STTup2 (STTup2 (MVectorFlp e1) (MVectorFlp e1)) (MVectorFlp e1) s1) p e s a

-- | Type alias for a ParST state of (Vec, (Vec, Vec))
type ParVec12T s1 e1 p e s a =
     ParST (STTup2 (MVectorFlp e1) (STTup2 (MVectorFlp e1) (MVectorFlp e1)) s1) p e s a

-- | Parallel merge kernel.
{-# INLINE pMergeTo2 #-}
pMergeTo2 :: (ParThreadSafe p, Ord e1, Show e1, PC.FutContents p (),
              PC.ParFuture p, HasGet e, HasPut e, PC.ParIVar p) =>
             Int -> SMerge -> ParVec21T s1 e1 p e s ()
pMergeTo2 threshold ma = do
  STTup2 (STTup2 (VFlp v1) (VFlp v2)) _ <- SS.get
  let l1 = MV.length v1
      l2 = MV.length v2

  if l1 == 0 && l2 == 0 then
    return ()
  else if l1 < threshold || l2 < threshold || l1 <= 1 || l2 <= 1 then do
    case ma of
      CMerge  -> cilkSeqMerge
      MPMerge -> seqmerge
  else do
    (splitL, splitR) <- findSplit
    let mid = splitL + splitR
    void $ forkSTSplit ((splitL, splitR), mid)
      (pMergeTo2 threshold ma)
      (pMergeTo2 threshold ma)

-- | Sequential merge kernel.
sMergeTo2 :: (ParThreadSafe p, Ord e1, Show e1) => ParVec21T s1 e1 p e s ()
sMergeTo2 = do
  (lenL, lenR) <- lengthLR1
  sMergeTo2K 0 lenL 0 lenR 0

sMergeTo2K :: (Ord a, ParThreadSafe p) =>
              Int -> Int -> Int -> Int -> Int -> ParST
              (STTup2 (STTup2 (SVectorFlp a) (SVectorFlp a)) (SVectorFlp a) s1) p e s ()
sMergeTo2K !lBot !lLen !rBot !rLen !index
  | lBot == lLen && rBot < rLen = do
    value <- indexR1 rBot
    write2 index value
    sMergeTo2K lBot lLen (rBot + 1) rLen (index + 1)

  | rBot >= rLen && lBot < lLen = do
    value <- indexL1 lBot
    write2 index value
    sMergeTo2K (lBot + 1) lLen rBot rLen (index + 1)

  | index >= (lLen + rLen) = do
    return ()

  | otherwise = do
    left <- indexL1 lBot
    right <- indexR1 rBot
    if left < right then do
      write2 index left
      sMergeTo2K (lBot + 1) lLen rBot rLen (index + 1)
     else do
      write2 index right
      sMergeTo2K lBot lLen (rBot + 1) rLen (index + 1)

-- | Mergeing from right-to-left works by swapping the states before
-- and after calling the left-to-right merge.
{-# INLINE mergeTo1 #-}
mergeTo1 sp threshold ma = do
  V.swapState
  mergeTo2 sp threshold ma
  V.swapState

-- NOTE(osa): This function assumes that at least one of the vectors are not
-- empty. Otherwise it fails with an error like:
-- *** Exception: ./Data/Vector/Generic/Mutable.hs:591 (read): index out of bounds (0,0)
--
{-# INLINE findSplit #-}
findSplit :: forall s1 e1 p e s .
             (ParThreadSafe p, Ord e1, Show e1,
              PC.ParMonad p) =>
             ParVec21T s1 e1 p e s (Int, Int)
findSplit = do
  STTup2 (STTup2 (VFlp vl) (VFlp vr)) _ <- SS.get
  let lLen = MV.length vl
      rLen = MV.length vr
  liftST $ findSplit' vl vr 0 lLen 0 rLen

findSplit' :: (MV.Storable a, Ord a) =>
              MV.MVector s a -> MV.MVector s a -> Int -> Int -> Int -> Int -> ST s (Int, Int)
findSplit' vl vr lLow lHigh rLow rHigh = do
  let lIndex = (lLow + lHigh) `div` 2
      rIndex = (rLow + rHigh) `div` 2

  left <- MV.read vl lIndex
  right <- MV.read vr rIndex
  if (lIndex == 0) && (rIndex == 0) then do
    return (lIndex, rIndex)
  else if lIndex == 0 then do
    rightSub1 <- MV.read vr (rIndex - 1)
    if rightSub1 <= left
       then return (lIndex, rIndex)
       else findSplit' vl vr 0 0 rLow rIndex
--           else trace ((show rIndex) ++ " " ++ (show lIndex) ++ "sp") $ split 0 0 rLow rIndex
  else if rIndex == 0 then do
    leftSub1 <- MV.read vl (lIndex - 1)
    if (leftSub1 <= right)
      then return (lIndex, rIndex)
      else findSplit' vl vr lLow lIndex 0 0
--          else trace ((show rIndex) ++ " " ++ (show lIndex) ++ "spl") $ split lLow lIndex 0 0
  else do
    rightSub1 <- MV.read vr (rIndex - 1)
    leftSub1 <- MV.read vl (lIndex - 1)
    if (leftSub1 <= right) && (rightSub1 <= left)
      then return (lIndex, rIndex)
      else if (leftSub1 <= right)
        then findSplit' vl vr lIndex lHigh rLow rIndex
        else findSplit' vl vr lLow lIndex rIndex rHigh

-----

{-# INLINE morphToVec21 #-}
morphToVec21 :: Int
             -> STTup2 (SVectorFlp v1) (SVectorFlp v2) s
             -> STTup2 (STTup2 (SVectorFlp v1) (SVectorFlp v1)) (SVectorFlp v2) s
morphToVec21 sp (STTup2 (VFlp vec1) vec2) =
  let l1 = MV.slice 0 sp vec1
      r1 = MV.slice sp (MV.length vec1 - sp) vec1 in
  STTup2 (STTup2 (VFlp l1) (VFlp r1)) vec2

morphToVec12 :: Int
             -> STTup2 (SVectorFlp v1) (SVectorFlp v2) s
             -> STTup2 (SVectorFlp v1) (STTup2 (SVectorFlp v2) (SVectorFlp v2)) s
{-# INLINE morphToVec12 #-}
morphToVec12 sp (STTup2 (VFlp vec1) (VFlp vec2)) =
  let l2 = MV.slice 0 sp vec2
      r2 = MV.slice sp (MV.length vec2 - sp) vec2 in
  STTup2 (VFlp vec1) (STTup2 (VFlp l2) (VFlp r2))

-----

{-# INLINE seqmerge #-}
seqmerge :: forall s1 e1 p e s . (ParThreadSafe p, Ord e1) => ParVec21T s1 e1 p e s ()
seqmerge = do
  STTup2 (STTup2 (VFlp left) (VFlp right)) (VFlp dest) <- SS.get

  let lenL = MV.length left
      lenR = MV.length right
      len = lenL + lenR

  let copyRemainingRight :: Int -> e1 -> Int -> ST s1 ()
      copyRemainingRight !ri !rx !di =
        if ri < (lenR-1) then do
          MV.write dest di rx
          let ri' = ri + 1
          rx' <- MV.read right ri'
          copyRemainingRight ri' rx' (di + 1)
        else when (di < len) $ do
          MV.write dest di rx

      copyRemainingLeft :: Int -> e1 -> Int -> ST s1 ()
      copyRemainingLeft !li !lx !di =
        if li < (lenL-1) then do
          MV.write dest di lx
          let li' = li + 1
          lx' <- MV.read left li'
          copyRemainingLeft li' lx' (di + 1)
        else when (di < len) $ do
          MV.write dest di lx

  let loop !li !lx !ri !rx !di =
        let di' = di+1 in
        if lx < rx then do
          MV.write dest di lx
          let li' = li + 1
          if li' == lenL then
            -- copy the rest of right into dest
            copyRemainingRight ri rx di'
          else when (di' < len) $ do
             lx' <- MV.read left li'
             loop li' lx' ri rx di'
        else do
          MV.write dest di rx
          let ri' = ri + 1
          if ri' == lenR then
            -- copy the rest of left into dest
            copyRemainingLeft li lx di'
          else when (di' < len) $ do
            rx' <- MV.read right ri'
            loop li lx ri' rx' di'

  if len == 0 then
    return ()
  else if lenL == 0 then liftST $ do
    fstR <- MV.read right 0
    copyRemainingRight 0 fstR 0
  else if lenR == 0 then liftST $ do
    fstL <- MV.read left 0
    copyRemainingLeft 0 fstL 0
  else liftST $ do
    fstL <- MV.read left 0
    fstR <- MV.read right 0
    loop 0 fstL 0 fstR 0

--------------------------------------------------------------------------------

-- Requires that we selected STORABLE vectors above!

foreign import ccall unsafe "wrap_seqquick"
  c_seqquick :: Ptr CElmT -> CLong -> IO (Ptr CElmT)

-- | Sequential Cilk sort, on the left vector, inplace.
cilkSeqSort :: (Ord eL, ParThreadSafe p, PC.ParMonad p) =>
               V.ParVec2T s1 eL eR p e s ()
-- This has the same signature & contract as seqSortL.
cilkSeqSort = do
  STTup2 (VFlp vecL) (VFlp _) <- SS.get
  internalLiftIO $ do
    let len = MV.length vecL
    let (fptr,_) = MV.unsafeToForeignPtr0 vecL
    withForeignPtr fptr $ \ vptr -> do
      -- No allocation, C operates on this memory in-place:
      _ <- c_seqquick (castPtr vptr) (fromIntegral len)
      return ()
    return ()

foreign import ccall unsafe "wrap_cilksort"
  c_cilksort ::  Ptr CElmT -> Ptr CElmT -> CLong -> IO CLong

foreign import ccall unsafe "wrap_seqmerge"
  c_seqmerge ::  Ptr CElmT -> CLong -> Ptr CElmT -> CLong -> Ptr CElmT -> IO ()

cilkSeqMerge :: (Ord e1, ParThreadSafe p, PC.ParMonad p) => ParVec21T s1 e1 p e s ()
cilkSeqMerge = do
  STTup2 (STTup2 (VFlp v1) (VFlp v2)) (VFlp v3) <- SS.get
  let (fptr1,_) = MV.unsafeToForeignPtr0 v1
      (fptr2,_) = MV.unsafeToForeignPtr0 v2
      (fptr3,_) = MV.unsafeToForeignPtr0 v3

  internalLiftIO $ do
    let len1 = MV.length v1
        len2 = MV.length v2
    withForeignPtr fptr1 $ \vptr1 ->
     withForeignPtr fptr2 $ \vptr2 ->
      withForeignPtr fptr3 $ \vptr3 ->
        c_seqmerge (castPtr vptr1) (fromIntegral len1)
                   (castPtr vptr2) (fromIntegral len2)
                   (castPtr vptr3)

-- Element type being sorted:
type CElmT = CInt

--------------------------------------------------------------------------------
-- Helpers for manipulating ParVec12T and ParVec21T

indexL1 :: (ParThreadSafe p) => Int -> ParVec21T s1 e1 p e s e1
indexL1 index = do
  STTup2 (STTup2 (VFlp l1) _) _ <- SS.get
  liftST $ MV.read l1 index

indexR1 :: (ParThreadSafe p) => Int -> ParVec21T s1 e1 p e s e1
indexR1 index = do
  STTup2 (STTup2 _ (VFlp r1)) _ <- SS.get
  liftST $ MV.read r1 index

indexL2 :: (ParThreadSafe p) => Int -> ParVec12T s1 e1 p e s e1
indexL2 index = do
  STTup2 _ (STTup2 (VFlp l2) _) <- SS.get
  liftST $ MV.read l2 index

indexR2 :: (ParThreadSafe p) => Int -> ParVec12T s1 e1 p e s e1
indexR2 index = do
  STTup2 _ (STTup2 _ (VFlp r2)) <- SS.get
  liftST $ MV.read r2 index

write1 :: (ParThreadSafe p) => Int -> e1 -> ParVec12T s1 e1 p e s ()
write1 index value = do
  STTup2 (VFlp v1) _ <- SS.get
  liftST $ MV.write v1 index value

write2 :: (ParThreadSafe p) => Int -> e1 -> ParVec21T s1 e1 p e s ()
write2 index value = do
  STTup2 _ (VFlp v2) <- SS.get
  liftST $ MV.write v2 index value

length2 :: (ParThreadSafe p) => ParVec21T s1 e1 p e s Int
length2 = do
  STTup2 _ (VFlp vec2) <- SS.get
  return $ MV.length vec2

length1 :: (ParThreadSafe p) => ParVec12T s1 e1 p e s Int
length1 = do
  STTup2 (VFlp v1) _ <- SS.get
  return $ MV.length v1

lengthLR1 :: (ParThreadSafe p) => ParVec21T s1 e1 p e s (Int, Int)
lengthLR1 = do
  STTup2 (STTup2 (VFlp vecL) (VFlp vecR)) _ <- SS.get
  let lenL = MV.length vecL
      lenR = MV.length vecR
  return (lenL, lenR)

lengthLR2 :: (ParThreadSafe p) => ParVec12T s1 s1 p e s (Int, Int)
lengthLR2 = do
  STTup2 _ (STTup2 (VFlp vecL) (VFlp vecR)) <- SS.get
  let lenL = MV.length vecL
      lenR = MV.length vecR
  return (lenL, lenR)