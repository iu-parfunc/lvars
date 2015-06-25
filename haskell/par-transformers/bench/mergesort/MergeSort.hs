{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Main(main) where

--module MergeSort
--       (
--         runTests
--       )
--       where

#ifdef PARSCHED
#warning "Using PARSCHED flag"
import           PARSCHED                     as LV
#else
--import Control.LVish      as LV
import           Control.Monad.Par            as LV
#endif
import qualified Control.Par.Class            as PC
import           Control.Par.Class.Unsafe     (ParThreadSafe (), internalLiftIO)
import           Control.Par.ST

import           Control.Monad
import           Control.Monad.ST             (ST)
import qualified Control.Monad.State.Strict   as SS
import           Data.Time.Clock

#ifdef BOXED
-- No reason to use the boxed version moving forward:
import qualified Control.Par.ST.Vec2          as V
import           Data.Vector                  (freeze)
import qualified Data.Vector                  as IMV
import           Data.Vector.Mutable          as MV
#elif defined(UNBOXED)
#warning "Using Unboxed Vectors."
#define VFlp UFlp
#define MVectorFlp UVectorFlp
import qualified Control.Par.ST.UVec2         as V
import           Data.Vector.Unboxed          (freeze)
import qualified Data.Vector.Unboxed          as IMV
import qualified Data.Vector.Unboxed.Mutable  as MV
#else
#warning "Using Storable Vectors."
#define VFlp SFlp
#define MVectorFlp SVectorFlp
import qualified Control.Par.ST.StorableVec2  as V
import qualified Data.Vector.Storable         as IMV
import qualified Data.Vector.Storable.Mutable as MV
#endif

-- [2013.11.15] Adding new variant:
import qualified Data.Vector.Algorithms.Intro as VI
import qualified Data.Vector.Algorithms.Merge as VA
import           Prelude                      hiding (length, read)
import qualified Prelude

import           System.Environment           (getArgs)
import           System.IO
import           System.Mem
import           System.Random.MWC            (create, uniformR)

import           Data.Int
-- import           Debug.Trace

--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  let (sz, st, mt, sa, ma, mode) = case args of
            []   -> (2^16, 2048, 2048, VAMSort, MPMerge, InPlace)
            [sz] -> (2^(Prelude.read sz), 2048, 2048, VAMSort, MPMerge, InPlace)

            [sz, st, mt, sa, ma] ->
                    (2^(Prelude.read sz), Prelude.read st, Prelude.read mt,
                     Prelude.read sa, Prelude.read ma, InPlace)

            [sz, st, mt, sa, ma, mode] ->
                    (2^(Prelude.read sz), Prelude.read st, Prelude.read mt,
                     Prelude.read sa, Prelude.read ma, Prelude.read mode)
  putStrLn $ wrapper sz st mt sa ma mode

data SMerge = CMerge | TMerge | MPMerge
  deriving (Show, Read)

data SSort = CSort | VAMSort | VAISort
  deriving (Show, Read)

data ParSort = InPlace | OutPlace
  deriving (Show, Read)

-- | Generate a random vector of length N and sort it using parallel
-- in-place merge sort.
{-# INLINE wrapper #-}
wrapper :: Int -> Int -> Int -> SSort -> SMerge -> ParSort -> String
wrapper size st mt sa ma mode =
  LV.runPar $ V.runParVec2T (0,size) $ computation size st mt sa ma mode

{-# INLINE computation #-}
computation :: (ParThreadSafe p, PC.ParMonad p, PC.FutContents p (),
                PC.ParFuture p, HasGet e, HasPut e, PC.ParIVar p) =>
               Int -> Int -> Int -> SSort -> SMerge -> ParSort ->
               V.ParVec2T s1 Int32 Int32 p e s String
computation size st mt sa ma mode = do

  -- test setup:
  randVec <- liftST $ mkRandomVec size

  -- WARNING: this is not safe! To do this with a safe API will require
  -- some proper "zooming" combinators, probably:
  STTup2 _ (VFlp right) <- SS.get
  SS.put (STTup2 (VFlp randVec) (VFlp right))

  internalLiftIO $ performGC
  internalLiftIO $ putStrLn "about to start timing"
  internalLiftIO $ hFlush stdout
  start <- internalLiftIO $ getCurrentTime
  -- post condition: left array is sorted
  case mode of
    InPlace  -> mergeSort st mt sa ma
    OutPlace -> mergeSortOutPlace st mt sa ma
  end <- internalLiftIO $ getCurrentTime

  internalLiftIO $ putStrLn "finished run"
  internalLiftIO $ hFlush stdout

  let runningTime = ((fromRational $ toRational $ diffUTCTime end start) :: Double)

  (rawL, rawR) <- V.reify
  frozenL <- liftST $ IMV.freeze rawL

  internalLiftIO $ putStrLn $ "Is Sorted?: " ++ show (checkSorted frozenL)
  internalLiftIO $ putStrLn $ "SELFTIMED: " ++ show runningTime

--  return $ show frozenL
  return $ "done"

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

  -- SET THRESHOLD
  if len < st then do
    case sa of
      VAMSort -> seqSortL
      VAISort -> seqSortL2
      CSort -> cilkSeqSort
      _ -> seqSortL
  else do
    let sp = (len `quot` 2)
    void $ forkSTSplit (sp,sp)
      (do len <- V.lengthL
          let sp = (len `quot` 2)
          void $ forkSTSplit (sp,sp)
            (mergeSort st mt sa ma)
            (mergeSort st mt sa ma)
          mergeTo2 sp mt ma)
      (do len <- V.lengthL
          let sp = (len `quot` 2)
          void $ forkSTSplit (sp,sp)
            (mergeSort st mt sa ma)
            (mergeSort st mt sa ma)
          mergeTo2 sp mt ma)
    mergeTo1 sp mt ma

mergeSortOutPlace ::
            (ParThreadSafe p, PC.FutContents p (),
              PC.ParFuture p, Ord e1, Show e1) =>
             Int -> Int -> SSort -> SMerge ->
             V.ParVec2T s1 e1 e1 p e s ()
mergeSortOutPlace !st !mt !sa !ma = do
  undefined
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
  STTup2 (VFlp vecL) (VFlp vecR) <- SS.get
  liftST $ VA.sort vecL

{-# INLINE seqSortL2 #-}
seqSortL2 :: (Ord eL, ParThreadSafe p) => V.ParVec2T s1 eL eR p e s ()
seqSortL2 = do
  STTup2 (VFlp vecL) (VFlp vecR) <- SS.get
  liftST $ VI.sort vecL

-- | Create a vector containing the numbers [0,N) in random order.
mkRandomVec :: Int -> ST s (MV.STVector s Int32)
mkRandomVec len =
  -- Annoyingly there is no MV.generate:
  do g <- create
     v <- IMV.thaw $ IMV.generate len fromIntegral
     loop 0 v g
     return v
 where
  -- Note: creating 2^24 elements takes 1.6 seconds under -O2 but 36
  -- seconds under -O0.  This sorely needs optimization!
  loop n vec g | n == len  = return vec
	       | otherwise = do
--    let (offset,g') = randomR (0, len - n - 1) g
    offset <- uniformR (0, len - n - 1) g
    MV.swap vec n (n + offset)
    loop (n+1) vec g

checkSorted :: IMV.Vector Int32 -> Bool
checkSorted vec = IMV.foldl' (\acc elem -> acc && elem) True $
                  IMV.imap (\i elem -> (fromIntegral i == elem)) vec



---------------

-- | Outer wrapper for a function that merges input vectors in the
-- left position into the vector in right position.
{-# INLINE mergeTo2 #-}
mergeTo2 :: (ParThreadSafe p, Ord e1, Show e1, PC.FutContents p (),
             PC.ParFuture p) =>
            Int -> Int -> SMerge -> V.ParVec2T s1 e1 e1 p e s ()
mergeTo2 sp threshold ma = do
  -- convert the state from (Vec, Vec) to ((Vec, Vec), Vec) then call normal parallel merge
  -- transmute (morphToVec21 sp) (pMergeTo2 threshold ma)
  undefined


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
  STTup2 (STTup2 (VFlp v1) (VFlp v2)) (VFlp v3) <- SS.get
  let l1 = MV.length v1
      l2 = MV.length v2
      len = MV.length v3

  if l1 < threshold || l2 < threshold then
    case ma of
      CMerge -> cilkSeqMerge
      MPMerge -> seqmerge
      _ -> seqmerge
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
  (mergeTo2 sp threshold ma)
  V.swapState

-- NOTE: THIS FUNCTION IS BORKED! It has issues with very small input
-- lengths and gets stuck. It might be a general problem with the
-- parallel merge algorithm, but is most likely something wrong about
-- how this function operates. We are being *very dangerous* and just
-- not using a small threshold that triggers this issue. t=4 breaks
-- it, but t=8 seems to work.
--
{-# INLINE findSplit #-}
findSplit :: forall s1 e1 p e s .
             (ParThreadSafe p, Ord e1, Show e1,
              PC.ParMonad p) =>
             ParVec21T s1 e1 p e s (Int, Int)
findSplit = do
  STTup2 (STTup2 (VFlp vl) (VFlp vr)) (VFlp v) <- SS.get
  let lLen = MV.length vl
      rLen = MV.length vr
      split :: Int -> Int -> Int -> Int -> ST s1 (Int, Int)
      split lLow lHigh rLow rHigh = do
        let lIndex = (lLow + lHigh) `div` 2
            rIndex = (rLow + rHigh) `div` 2

        left <- MV.read vl lIndex
        right <- MV.read vr rIndex
        if (lIndex == 0) && (rIndex == 0) then do
          return (lIndex, rIndex)
        else if (lIndex == 0) then do
          rightSub1 <- MV.read vr (rIndex - 1)
          if (rightSub1 <= left)
             then return (lIndex, rIndex)
             else split 0 0 rLow rIndex
--             else trace ((show rIndex) ++ " " ++ (show lIndex) ++ "sp") $ split 0 0 rLow rIndex
        else if (rIndex == 0) then do
          leftSub1 <- MV.read vl (lIndex - 1)
          if (leftSub1 <= right)
            then return (lIndex, rIndex)
            else split lLow lIndex 0 0
--            else trace ((show rIndex) ++ " " ++ (show lIndex) ++ "spl") $ split lLow lIndex 0 0
        else do
          rightSub1 <- MV.read vr (rIndex - 1)
          leftSub1 <- MV.read vl (lIndex - 1)
          if (leftSub1 <= right) && (rightSub1 <= left)
            then return (lIndex, rIndex)
            else if (leftSub1 <= right)
              then split lIndex lHigh rLow rIndex
              else split lLow lIndex rIndex rHigh
--  ans <- trace "splt" $ liftST$ split 0 lLen 0 rLen
--  trace (show lLen ++ " " ++ show rLen ++ " ret ans " ++ show ans) $ return ans
  liftST $ split 0 lLen 0 rLen

-----

{-# INLINE morphToVec21 #-}
morphToVec21 :: Int
             -> STTup2 (SVectorFlp v1) (SVectorFlp v2) s
             -> STTup2 (STTup2 (SVectorFlp v1) (SVectorFlp v1)) (SVectorFlp v2) s
morphToVec21 sp (STTup2 (VFlp vec1) (VFlp vec2)) =
  let l1 = MV.slice 0 sp vec1
      r1 = MV.slice sp (MV.length vec1 - sp) vec1 in
  STTup2 (STTup2 (VFlp l1) (VFlp r1)) (VFlp vec2)

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

  {- INLINE copyRemainingRight #-}
  {- INLINE copyRemainingLeft #-}
  {- INLINE loop #-}

  let copyRemainingRight :: Int -> e1 -> Int -> ST s1 ()
      copyRemainingRight !ri !rx !di =
        if ri < (lenR-1) then do
          MV.write dest di rx
          let ri' = ri + 1
          rx' <- MV.read right ri'
          copyRemainingRight ri' rx' (di + 1)
        else when (di < len) $ do
          MV.write dest di rx
          return ()
      copyRemainingLeft !li !lx !di =
        if li < (lenL-1) then do
          MV.write dest di lx
          let li' = li + 1
          lx' <- MV.read left li'
          copyRemainingLeft li' lx' (di + 1)
        else when (di < len) $ do
          MV.write dest di lx
          return ()

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
  fstL <- liftST$ MV.read left 0
  fstR <- liftST$ MV.read right 0
  liftST $ loop 0 fstL 0 fstR 0
  return ()

--------------------------------------------------------------------------------

#ifdef CILK_SEQ
#warning "CILK_SEQ defined"

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
      c_seqquick (castPtr vptr) (fromIntegral len)
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
type ElmT  = Word32
type CElmT = CUInt
#else
#warning "CILK_SEQ isn't defined"

cilkSeqSort = undefined
cilkSeqMerge = undefined

#endif

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

lengthLR2 :: (ParThreadSafe p) => ParVec12T s1 s1 p e s (Int,Int)
lengthLR2 = do
  STTup2 _ (STTup2 (VFlp vecL) (VFlp vecR)) <- SS.get
  let lenL = MV.length vecL
      lenR = MV.length vecR
  return (lenL, lenR)
