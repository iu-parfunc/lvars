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
{-# LANGUAGE CPP #-}



module Main where

--module MergeSort 
--       (
--         runTests
--       )
--       where

import Control.LVish      as LV
import qualified Data.LVar.IVar as IV
import Control.Par.StateT as S

import Control.Monad
import Control.Monad.IO.Class
import qualified Control.Monad.Trans.State.Strict as S
import Control.Monad.ST        (ST)
import Control.Monad.ST.Unsafe (unsafeSTToIO)
import Control.Monad.Trans (lift)
import Control.Monad.State (get,put)

import Control.Concurrent (threadDelay)

import Data.STRef
#ifdef UNBOXED

#else
import Data.Vector.Mutable as MV
import Data.Vector       (freeze)
import qualified Data.Vector as IMV
#endif

import qualified Data.Vector.Algorithms.Intro as VA
import Prelude hiding (read, length)
import System.IO.Unsafe (unsafePerformIO)

import GHC.Prim (RealWorld)

import qualified Control.Par.Class as PC

import Test.HUnit (Assertion, assert, assertEqual, assertBool, Counts(..))
import Test.Framework.Providers.HUnit
import Test.Framework -- (Test, defaultMain, testGroup)
import Test.Framework.TH (defaultMainGenerator)

import qualified Control.Par.ST.Vec2 as V
import qualified Control.Par.ST as PST

import qualified Control.Monad.State.Strict as SS

import Control.Par.Class.Unsafe (ParThreadSafe(unsafeParIO))

import System.Random.MWC (create, uniformVector, uniformR)

import Debug.Trace

runTests :: Bool
runTests = True

wrapper :: String
wrapper = LV.runPar $ V.runParVec2T (17,17) $ do
  -- hack: put our input vector into the state
--  vecR <- PST.liftST$ MV.new $ length vecL
--  randVec <- SS.liftIO$ mkRandomVec 10
--  SS.put (PST.STTup2 (PST.VFlp vecL) (PST.VFlp vecR))  
  
  V.setL 1.0
  V.setR 0.1
  
  -- toy vector to start with
  V.writeL 0 120.0
  V.writeL 1 5.0
  V.writeL 2 3.0
  V.writeL 3 0.222
  
  -- input: unsorted array in Left position
  -- output: sorted array in Left position
  mergeSort
    
  (rawL, rawR) <- V.reify
  frozenL <- PST.liftST$ freeze rawL
  
  return$ show frozenL

mergeSort :: (ParThreadSafe parM, PC.FutContents parM (),
              PC.ParFuture parM, Ord elt, Show elt) => 
             V.ParVec2T s1 elt elt parM ()  
mergeSort = do
  len <- V.lengthL
  
  if len < 4 then do
    seqSortL
   else do  
    let sp = (len `quot` 2)              
    PST.forkSTSplit (sp,sp)
      (do len <- V.lengthL
          let sp = (len `quot` 2)
          PST.forkSTSplit (sp,sp)
            mergeSort
            mergeSort
          mergeTo2 4 sp)
      (do len <- V.lengthL                                                                    
          let sp = (len `quot` 2)                                                              
          PST.forkSTSplit (sp,sp)                             
            mergeSort         
            mergeSort
          mergeTo2 4 sp)                           
    mergeTo1 sp
    

seqSortL :: (Ord eltL, ParThreadSafe parM) => V.ParVec2T s eltL eltR parM ()
seqSortL = do
  PST.STTup2 (PST.VFlp vecL) (PST.VFlp vecR) <- SS.get
  PST.liftST$ VA.sort vecL

main = putStrLn wrapper

-- | Create a vector containing the numbers [0,N) in random order.
mkRandomVec :: Int -> IO (MV.IOVector Float)
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

---------------

mergeTo2 :: (ParThreadSafe parM, Ord elt, Show elt, PC.FutContents parM (),
                PC.ParFuture parM) => 
                Int -> Int -> V.ParVec2T s elt elt parM ()
mergeTo2 sp threshold = do
  -- convert the state from (Vec, Vec) to ((Vec, Vec), Vec) then call normal parallel merge
  PST.transmute (\(PST.STTup2 (PST.VFlp vec1) (PST.VFlp vec2)) ->
                  let l1 = MV.slice 0 sp vec1
                      r1 = MV.slice sp (MV.length vec1 - sp) vec1 in
                  PST.STTup2 (PST.STTup2 (PST.VFlp l1) (PST.VFlp r1)) (PST.VFlp vec2))
     (pMergeTo2 threshold)
                  
pMergeTo2 :: (ParThreadSafe parM, Ord elt, Show elt, PC.FutContents parM (),
           PC.ParFuture parM) =>
          Int -> ParVec21T s elt parM ()
pMergeTo2 threshold = do
  -- threshold check here
  PST.STTup2 (PST.STTup2 (PST.VFlp vecL) (PST.VFlp vecR)) (PST.VFlp vec2) <- SS.get
  let len = MV.length vec2
  if len < threshold then
    sMergeTo2
   else do
    -- find the split points
    let mid = len `div` 2
    (splitL, splitR) <- findSplit indexL1 indexL1
    
    PST.forkSTSplit ((splitL, splitR), mid)
      (pMergeTo2 threshold)
      (pMergeTo2 threshold)
    return ()
      
sMergeTo2 :: (ParThreadSafe parM, Ord elt, Show elt) =>       
            ParVec21T s elt parM ()
sMergeTo2 = do
  PST.STTup2 (PST.STTup2 (PST.VFlp vecL) (PST.VFlp vecR)) (PST.VFlp vec2) <- SS.get
  let lenL = MV.length vecL
      lenR = MV.length vecR
  sMergeTo2K 0 lenL 0 lenR 0
      
sMergeTo2K lBot lLen rBot rLen index 
  | lBot == lLen && rBot <= rLen = do
    value <- indexR1 rBot
    write2 index value
    sMergeTo2K lBot lLen (rBot + 1) rLen (index + 1)      

sMergeTo2K lBot lLen rBot rLen index 
  | rBot > rLen && lBot < lLen = do
    value <- indexL1 lBot
    write2 index value
    sMergeTo2K (lBot + 1) lLen rBot rLen (index + 1)
    
sMergeTo2K lBot lLen rBot rLen index     
  | index > (lLen + rLen) = do
    return ()

sMergeTo2K lBot lLen rBot rLen index = do
  left <- indexL1 lBot
  right <- indexR1 rBot
  if left < right then do
    write2 index left
    sMergeTo2K (lBot + 1) lLen rBot rLen (index + 1)
   else do
    write2 index right
    sMergeTo2K lBot lLen (rBot + 1) rLen (index + 1)    
    
mergeTo1 = undefined    
    
findSplit :: (ParThreadSafe parM, Ord elt, Show elt) => 
             STIndexFunction s elt parM -> STIndexFunction s elt parM ->
             ParVec21T s elt parM (Int, Int)

findSplit indexLeft indexRight = do                                 
    PST.STTup2 (PST.STTup2 (PST.VFlp vecL) (PST.VFlp vecR)) (PST.VFlp vec2) <- SS.get
    let lLen = MV.length vecL
        rLen = MV.length vecR
    
    split 0 lLen 0 rLen 
      where

        split lLow lHigh rLow rHigh = do
          let lIndex = (lLow + lHigh) `div` 2
              rIndex = (rLow + rHigh) `div` 2
            
          leftSub1 <- indexLeft (lIndex - 1)
          left <- indexLeft lIndex
          rightSub1 <- indexRight (rIndex - 1)
          right <- indexRight rIndex
        
          if (lIndex == 0)
          then if (rightSub1 < left)
            then return (lIndex, rIndex)
            else split 0 0 rLow rIndex
          else if (rIndex == 0)
          then if (leftSub1 < right)
            then return (lIndex, rIndex)
            else split lLow lIndex 0 0
          else if (leftSub1 < right) && (rightSub1 < left)
            then return (lIndex, rIndex)
            else if (leftSub1 < right)
              then split lIndex lHigh rLow rIndex
              else split lLow lIndex rIndex rHigh
                
      
type ParVec21T s elt parM ans = PST.ParST (PST.STTup2 (PST.STTup2 (PST.MVectorFlp elt) 
                                                       (PST.MVectorFlp elt))
                                           (PST.MVectorFlp elt) s) parM ans
                                              
type STIndexFunction s elt parM = Int -> ParVec21T s elt parM elt 
                       
                       
indexL1 :: (ParThreadSafe parM, Ord elt, Show elt) => STIndexFunction s elt parM
indexL1 = undefined

indexR1 :: (ParThreadSafe parM, Ord elt, Show elt) => STIndexFunction s elt parM
indexR1 = undefined
             
lengthR :: (ParThreadSafe parM) => ParVec21T s elt parM Int      
lengthR = do
    PST.STTup2 (PST.STTup2 (PST.VFlp vecL) (PST.VFlp vecR)) (PST.VFlp vec2) <- SS.get
    return $ MV.length vec2
        
write2 :: (ParThreadSafe parM) => Int -> elt -> ParVec21T s elt parM ()
write2 = undefined
      
