{-# LANGUAGE MultiParamTypeClasses #-}                                                         
{-# LANGUAGE ScopedTypeVariables #-}                                                           
{-# LANGUAGE GeneralizedNewtypeDeriving #-}                                                    
{-# LANGUAGE TypeFamilies #-}                                                                  
{-# LANGUAGE ConstraintKinds #-}                                                               
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}                                                             
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Main(main) where

--module MergeSort 
--       (
--         runTests
--       )
--       where

#ifdef PARSCHED
#warning "Using PARSCHED flag"
import PARSCHED as LV
#else
--import Control.LVish      as LV
import Control.Monad.Par      as LV
#endif
import Control.Par.ST
import Control.Par.Class.Unsafe (ParThreadSafe(), internalLiftIO)
import qualified Control.Par.Class as PC

import Control.Monad
import Control.Monad.ST        (ST)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.State.Strict as SS
import Data.Word
import Data.Time.Clock

#ifdef BOXED
-- No reason to use the boxed version moving forward:
import Data.Vector.Mutable as MV
import qualified Data.Vector as IMV
import qualified Control.Par.ST.Vec2 as V
import Data.Vector (freeze)
#elif defined(UNBOXED)
#warning "Using Unboxed Vectors."
#define VFlp UFlp
#define MVectorFlp UVectorFlp
import qualified Data.Vector.Unboxed as IMV
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Control.Par.ST.UVec2 as V
import Data.Vector.Unboxed (freeze)
#else
#warning "Using Storable Vectors."
#define VFlp SFlp
#define MVectorFlp SVectorFlp
import qualified Data.Vector.Storable as IMV
import qualified Data.Vector.Storable.Mutable as MV
import qualified Control.Par.ST.StorableVec2 as V
import Foreign.Ptr
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.C.Types
import Foreign.Marshal.Array (allocaArray)
#endif

-- [2013.11.15] Adding new variant:
import qualified Data.Vector.Algorithms.Intro as VI
import qualified Data.Vector.Algorithms.Merge as VA
import Prelude hiding (read, length)
import qualified Prelude

import System.Random.MWC (create, uniformR) -- uniformVector,
import System.Environment (getArgs)
import System.Mem
import System.IO.Unsafe (unsafePerformIO)
import System.IO
import Control.Monad.ST.Unsafe (unsafeIOToST)

import Data.Int
import Control.Exception (assert)
import Debug.Trace

--------------------------------------------------------------------------------

#define SAFE
#ifndef SAFE
thawit  x     = IMV.unsafeThaw   x
newMV   x     = MV.unsafeNew   x
readMV  x y   = MV.unsafeRead  x y
writeMV x y z = MV.unsafeWrite x y z
sliceMV x y z = MV.unsafeSlice x y z
copyMV  x y   = MV.unsafeCopy  x y 
#else
thawit  x     = IMV.thaw   x
newMV   x     = MV.new   x
readMV  x y   = MV.read  x y
writeMV x y z = MV.write x y z
sliceMV x y z = MV.slice x y z
copyMV  x y   = MV.copy  x y 
#endif
{-# INLINE thawit #-}
{-# INLINE newMV #-}
{-# INLINE readMV #-}
{-# INLINE writeMV #-}
{-# INLINE sliceMV #-}
{-# INLINE copyMV #-}

--------------------------------------------------------------------------------

{-# INLINE runTests #-}
runTests :: Bool
runTests = True

-- | Generate a random vector of length N and sort it using parallel
-- in-place merge sort. 
{-# INLINE wrapper #-}
wrapper :: Int -> Int -> Int -> SSort -> SMerge -> ParSort -> String
wrapper size st mt sa ma mode =
  LV.runPar $ V.runParVec2T (0,size) $ 
  computation size st mt sa ma mode

{-# INLINE computation #-}
computation :: (ParThreadSafe parM, PC.ParMonad parM, PC.FutContents parM (), 
                PC.ParFuture parM) => 
               Int -> Int -> Int -> SSort -> SMerge -> ParSort -> 
               V.ParVec2T s Int32 Int32 parM String
computation size st mt sa ma mode = do

  -- test setup: 
  randVec <- liftST$ mkRandomVec size    
  
  STTup2 (VFlp left) (VFlp right) <- SS.get
  SS.put (STTup2 (VFlp randVec) (VFlp right))

  internalLiftIO$ performGC
  internalLiftIO$ putStrLn "about to start timing"  
  internalLiftIO$ hFlush stdout
  start <- internalLiftIO$ getCurrentTime  
  -- post condition: left array is sorted
  case mode of
    InPlace  -> mergeSort st mt sa ma 
    OutPlace -> mergeSortOutPlace st mt sa ma 
  end <- internalLiftIO$ getCurrentTime

  internalLiftIO$ putStrLn "finished run"
  internalLiftIO$ hFlush stdout
  
  let runningTime = ((fromRational $ toRational $ diffUTCTime end start) :: Double)
    
  (rawL, rawR) <- V.reify
  frozenL <- liftST$ IMV.freeze rawL
  
  internalLiftIO$ putStrLn$ "Is Sorted?: "++show (checkSorted frozenL)
  internalLiftIO$ putStrLn$ "SELFTIMED: "++show runningTime  
  
--  return $ show frozenL
  return$ "done"

-- | Given a vector in left position, and an available buffer of equal
-- size in the right position, sort the left vector.
{-# INLINE mergeSort #-}
mergeSort :: (ParThreadSafe parM, PC.FutContents parM (),
              PC.ParFuture parM, Ord elt, Show elt) => 
             Int -> Int -> SSort -> SMerge -> 
             V.ParVec2T s1 elt elt parM ()  
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
    forkSTSplit (sp,sp)
      (do len <- V.lengthL
          let sp = (len `quot` 2)
          forkSTSplit (sp,sp)
            (mergeSort st mt sa ma)
            (mergeSort st mt sa ma)
          mergeTo2 sp mt ma)
      (do len <- V.lengthL                                                                    
          let sp = (len `quot` 2)                                                              
          forkSTSplit (sp,sp)
            (mergeSort st mt sa ma)        
            (mergeSort st mt sa ma)
          mergeTo2 sp mt ma)
    mergeTo1 sp mt ma

mergeSortOutPlace ::
            (ParThreadSafe parM, PC.FutContents parM (),
              PC.ParFuture parM, Ord elt, Show elt) => 
             Int -> Int -> SSort -> SMerge -> 
             V.ParVec2T s1 elt elt parM ()  
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
seqSortL :: (Ord eltL, ParThreadSafe parM) => V.ParVec2T s eltL eltR parM ()
seqSortL = do
  STTup2 (VFlp vecL) (VFlp vecR) <- SS.get
  liftST$ VA.sort vecL

{-# INLINE seqSortL2 #-}
seqSortL2 :: (Ord eltL, ParThreadSafe parM) => V.ParVec2T s eltL eltR parM ()
seqSortL2 = do
  STTup2 (VFlp vecL) (VFlp vecR) <- SS.get
  liftST$ VI.sort vecL


data SMerge = CMerge | TMerge | MPMerge
  deriving (Show, Read)
data SSort = CSort | VAMSort | VAISort
  deriving (Show, Read)

data ParSort = InPlace | OutPlace
  deriving (Show, Read)

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
mergeTo2 :: (ParThreadSafe parM, Ord elt, Show elt, PC.FutContents parM (),
             PC.ParFuture parM) => 
            Int -> Int -> SMerge -> V.ParVec2T s elt elt parM ()
mergeTo2 sp threshold ma = do
  -- convert the state from (Vec, Vec) to ((Vec, Vec), Vec) then call normal parallel merge
  transmute (morphToVec21 sp) (pMergeTo2 threshold ma)
                  
-- | Parallel merge kernel.
{-# INLINE pMergeTo2 #-}
pMergeTo2 :: (ParThreadSafe parM, Ord elt, Show elt, PC.FutContents parM (),
              PC.ParFuture parM) =>
             Int -> SMerge -> ParVec21T s elt parM ()
pMergeTo2 threshold ma = do
--  len <- length1
  STTup2 (STTup2 (VFlp v1) (VFlp v2)) (VFlp v3) <- SS.get
  let l1 = MV.length v1
      l2 = MV.length v2  
      len = MV.length v3

--  if len < threshold then
  if l1 < threshold || l2 < threshold then 
    case ma of 
      TMerge -> sMergeTo2
      CMerge -> cilkSeqMerge
      MPMerge -> seqmerge
      _ -> sMergeTo2
   else do
    (splitL, splitR) <- findSplit
    let mid = splitL + splitR
    forkSTSplit ((splitL, splitR), mid)
      (pMergeTo2 threshold ma)
      (pMergeTo2 threshold ma)
    return ()
      
-- | Sequential merge kernel.      
sMergeTo2 :: (ParThreadSafe parM, Ord elt, Show elt) =>       
            ParVec21T s elt parM ()
sMergeTo2 = do
  (lenL, lenR) <- lengthLR1
  sMergeTo2K 0 lenL 0 lenR 0
      
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
findSplit :: forall s elt parM . (ParThreadSafe parM, Ord elt, Show elt,
              PC.ParMonad parM) => 
--             (Int -> ParVec21T s elt parM elt) ->
--             (Int -> ParVec21T s elt parM elt)->
             ParVec21T s elt parM (Int, Int)

--findSplit indexLeft indexRight = do                                 
findSplit = do
  
  --(lLen, rLen) <- lengthLR1    
  STTup2 (STTup2 (VFlp vl) (VFlp vr)) (VFlp v) <- SS.get
  let lLen = MV.length vl
      rLen = MV.length vr  
      split :: Int -> Int -> Int -> Int -> ST s (Int, Int)
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
  liftST$ split 0 lLen 0 rLen

-- | Type alias for a ParST state of ((Vec,Vec), Vec)             
type ParVec21T s elt parM ans = ParST (STTup2 
                                       (STTup2 (MVectorFlp elt) 
                                               (MVectorFlp elt))
                                       (MVectorFlp elt) s)
                                      parM ans
                                              
-- | Type alias for a ParST state of (Vec, (Vec, Vec))                                
type ParVec12T s elt parM ans = ParST (STTup2 
                                       (MVectorFlp elt)
                                       (STTup2 (MVectorFlp elt) 
                                               (MVectorFlp elt)) s)
                                      parM ans
    
                       
-- | Helpers for manipulating ParVec12T and ParVec21T                                
                                
indexL1 :: (ParThreadSafe parM) => Int -> ParVec21T s elt parM elt
indexL1 index = do
  STTup2 (STTup2 (VFlp l1) (VFlp r1)) (VFlp v2) <- SS.get
  liftST$ MV.read l1 index

indexR1 :: (ParThreadSafe parM) => Int -> ParVec21T s elt parM elt
indexR1 index = do
  STTup2 (STTup2 (VFlp l1) (VFlp r1)) (VFlp v2) <- SS.get
  liftST$ MV.read r1 index

indexL2 :: (ParThreadSafe parM) => Int -> ParVec12T s elt parM elt
indexL2 index = do
  STTup2 (VFlp v1) (STTup2 (VFlp l2) (VFlp r2)) <- SS.get
  liftST$ MV.read l2 index

indexR2 :: (ParThreadSafe parM) => Int -> ParVec12T s elt parM elt
indexR2 index = do
  STTup2 (VFlp v1) (STTup2 (VFlp l2) (VFlp r2)) <- SS.get
  liftST$ MV.read r2 index

write1 :: (ParThreadSafe parM) => Int -> elt -> ParVec12T s elt parM ()
write1 index value = do
  STTup2 (VFlp v1) (STTup2 (VFlp l2) (VFlp r2)) <- SS.get
  liftST$ MV.write v1 index value
             
write2 :: (ParThreadSafe parM) => Int -> elt -> ParVec21T s elt parM ()
write2 index value = do
  STTup2 (STTup2 (VFlp l1) (VFlp r1)) (VFlp v2) <- SS.get
  liftST$ MV.write v2 index value

length2 :: (ParThreadSafe parM) => ParVec21T s elt parM Int      
length2 = do
  STTup2 (STTup2 (VFlp vecL) (VFlp vecR)) (VFlp vec2) <- SS.get
  return $ MV.length vec2
        
length1 :: (ParThreadSafe parM) => ParVec12T s elt parM Int
length1 = do
  STTup2 (VFlp v1) (STTup2 (VFlp l2) (VFlp r2)) <- SS.get
  return$ MV.length v1
      
lengthLR1 :: (ParThreadSafe parM) => ParVec21T s elt parM (Int,Int)
lengthLR1 = do
  STTup2 (STTup2 (VFlp vecL) (VFlp vecR)) (VFlp vec2) <- SS.get
  let lenL = MV.length vecL
      lenR = MV.length vecR
  return$ (lenL, lenR)

lengthLR2 :: (ParThreadSafe parM) => ParVec12T s elt parM (Int,Int)
lengthLR2 = do
  STTup2 (VFlp v1) (STTup2 (VFlp vecL) (VFlp vecR)) <- SS.get
  let lenL = MV.length vecL
      lenR = MV.length vecR
  return$ (lenL, lenR)

-----
  
{-# INLINE morphToVec21 #-}
morphToVec21 sp (STTup2 (VFlp vec1) (VFlp vec2)) =
  let l1 = MV.slice 0 sp vec1
      r1 = MV.slice sp (MV.length vec1 - sp) vec1 in
  STTup2 (STTup2 (VFlp l1) (VFlp r1)) (VFlp vec2)

morphToVec12 sp (STTup2 (VFlp vec1) (VFlp vec2)) =
  let l2 = MV.slice 0 sp vec2
      r2 = MV.slice sp (MV.length vec2 - sp) vec2 in
  STTup2 (VFlp vec1) (STTup2 (VFlp l2) (VFlp r2))

-----

vec2printState :: (ParThreadSafe parM, Show elt, PC.ParFuture parM,
                   PC.FutContents parM ()) =>
                  String -> V.ParVec2T s elt elt parM ()
vec2printState str = do
  STTup2 (VFlp v1) (VFlp v2) <- SS.get
  f1 <- liftST$ IMV.freeze v1
  f2 <- liftST$ IMV.freeze v2
  internalLiftIO$ putStrLn$ str ++ " " ++ show f1 ++ " " ++ show f2
  
vec21printState :: (ParThreadSafe parM, Show elt, PC.ParMonad parM) =>
                  String -> ParVec21T s elt parM ()
vec21printState str = do
  STTup2 (STTup2 (VFlp v1) (VFlp v2)) (VFlp v3) <- SS.get
  f1 <- liftST$ IMV.freeze v1
  f2 <- liftST$ IMV.freeze v2
  f3 <- liftST$ IMV.freeze v3
  internalLiftIO$ putStrLn$ str ++ " (" ++ show f1 ++ ", " ++ show f2 ++ "), " ++ show f3 ++ ")"

-----------
  
{-# INLINE seqmerge #-}
seqmerge :: forall elt parM s . (ParThreadSafe parM, Ord elt) => ParVec21T s elt parM ()
seqmerge = do
  STTup2 (STTup2 (VFlp left) (VFlp right)) (VFlp dest) <- SS.get
  
  let lenL = MV.length left                                                   
      lenR = MV.length right
      len = lenL + lenR

--  assert (len == MV.length dest) $ return ()  
  {- INLINE copyRemainingRight #-}
  {- INLINE copyRemainingLeft #-}
  {- INLINE loop #-}
      
  let copyRemainingRight :: Int -> elt -> Int -> ST s ()
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
  liftST$ loop 0 fstL 0 fstR 0
  return ()                           

  

--------------------------------------------------------------------------------
#define CILK_SEQ
#ifdef CILK_SEQ
-- Requires that we selected STORABLE vectors above!
    
foreign import ccall unsafe "wrap_seqquick"
  c_seqquick :: Ptr CElmT -> CLong -> IO (Ptr CElmT)

-- | Sequential Cilk sort, on the left vector, inplace.
cilkSeqSort :: (Ord eltL, ParThreadSafe parM, PC.ParMonad parM) =>
               V.ParVec2T s eltL eltR parM ()
-- This has the same signature & contract as seqSortL.
cilkSeqSort = do
  STTup2 (VFlp vecL) (VFlp _) <- SS.get
  internalLiftIO$ do
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

cilkSeqMerge :: (Ord elt, ParThreadSafe parM, PC.ParMonad parM) =>
                ParVec21T s elt parM ()
cilkSeqMerge = do
  STTup2 (STTup2 (VFlp v1) (VFlp v2)) (VFlp v3) <- SS.get
  let (fptr1,_) = MV.unsafeToForeignPtr0 v1
      (fptr2,_) = MV.unsafeToForeignPtr0 v2
      (fptr3,_) = MV.unsafeToForeignPtr0 v3

  internalLiftIO$ do
    let len1 = MV.length v1  
        len2 = MV.length v2
    withForeignPtr fptr1 $ \vptr1 ->
     withForeignPtr fptr2 $ \vptr2 ->
      withForeignPtr fptr3 $ \vptr3 ->
        c_seqmerge (castPtr vptr1) (fromIntegral len1)
                   (castPtr vptr2) (fromIntegral len2)
                   (castPtr vptr3)
  return ()

-- Element type being sorted:
type ElmT  = Word32
type CElmT = CUInt
#else
cilkSeqSort = undefined
cilkSeqMerge = undefined
#endif
-- End CILK block.
  
