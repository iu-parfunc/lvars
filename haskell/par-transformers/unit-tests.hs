{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.LVish      as LV
import Control.LVish.Internal ()
import qualified Data.LVar.IVar as IV
import Control.Par.StateT as S

import Control.Monad
import Control.Monad.IO.Class
import qualified Control.Monad.Trans.State.Strict as S
import Control.Monad.ST        (ST)
import Control.Monad.ST.Unsafe (unsafeSTToIO)
import Control.Monad.Trans (lift)

import Control.Concurrent (threadDelay)

import Data.STRef
import Data.Vector.Mutable as MV
import Data.Vector       (freeze)
import Prelude hiding (read, length)
import System.IO.Unsafe (unsafePerformIO)

import GHC.Prim (RealWorld)

import qualified Control.Par.Class as PC

import Test.HUnit (Assertion, assert, assertEqual, assertBool, Counts(..))
import Test.Framework.Providers.HUnit
import Test.Framework -- (Test, defaultMain, testGroup)
import Test.Framework.TH (defaultMainGenerator)

import Control.Par.VecT as V1
import qualified Control.LVish.ParVec as V2

--------------------------------------------------------------------------------
-- Tests for VecT (V1)
--------------------------------------------------------------------------------

case_t2 :: Assertion
case_t2 = assertEqual "basic forkWithVec usage"
            "hello fromList [0.0,0.0,35.3,0.0,0.0,0.0,0.0,46.3,0.0,0.0]" t2

t2 :: String
t2 = LV.runPar $
     runVecT p2

-- | A simple test that modifies two locations in a vector, multiple times, in parallel.
p2 :: VecT s Float (LV.Par d s2) String
p2 = do
  r <- V1.liftST$ newSTRef "hi"
  initVecT 10
  v0 <- getVecT

  V1.liftST$ set v0 0

  forkWithVec 5
     (do v1 <- getVecT
         -- We can't protect against this sort of out-of-bounds error
         -- at compile time -- for that we'd need dependent types.
         -- V1.liftST$ write v1 9 0 -- BAD! out of bounds
         V1.liftST$ do write v1 2 33.3
                       tmp <- read v1 2
                       write v1 2 (tmp + 2)
     )
     (do v2 <- getVecT
         -- This, we actually *can* protect against at compile time.
         -- V1.liftST$ read v 2  -- BAD!
         -- V1.liftST$ readSTRef r
         V1.liftST$ do write v2 2 44.3
                       tmp <- read v2 2
                       write v2 2 (tmp + 2)         
     )

  -- After the barrier we can access v0 again:
  z <- V1.liftST$ freeze v0

  V1.liftST$ writeSTRef r "hello "
  hello <- V1.liftST$ readSTRef r
  return$ hello ++ show z

-- | Attempt to stack a VecT ontop of another VecT
p3 :: VecT s2 Float (VecT s1 Int (LV.Par d s0)) String 
p3 = do
  initVecT 10
  vo <- getVecT
  vi <- lift$ do
    initVecT 20
    vi <- getVecT            
    V1.liftST$ write vi 0 5
    return vi
  
  V1.liftST$ write vo 0 120.0
    
  -- this line doesn't have a meaning, it is just here to make sure it
  -- typechecks
  let len = length vi
  
  voh <- V1.liftST$ read vo 0
  vih <- lift$ V1.liftST$ read vi 0
  
  return$ show voh ++ show vih
  
t3 :: String
t3 = LV.runPar $
     runVecT $ runVecT p3
     
case_t3 :: Assertion
case_t3 = assertEqual "simple stacked VecT"
          "120.05" t3



-- | Given a vector of "unknown" length, find the length.
printLength :: VecT s Float (LV.Par d s2) String
printLength = do
  initVecT 120
  v <- getVecT
  let len = (length v)
  return$ show len

t4 :: String
t4 = LV.runPar $ runVecT printLength

case_t4 :: Assertion
case_t4 = assertEqual "test fetching a vector length"
          "120" t4
       
-- | Try to forkWithVec on a stack of VecT
-- 
-- BUG: This could output two different results depending on data races.
p5 :: VecT s2 Int (VecT s1 Int (LV.Par d s0)) String
p5 = do
  initVecT 10
  vo <- getVecT
  V1.liftST$ set vo 0
  
  lift$ initVecT 10
  vi <- lift$ getVecT
  lift$ V1.liftST$ set vi 1
  
  forkWithVec 5
    (do vo1 <- getVecT
        V1.liftST$ do write vo1 0 5
        -- liftIO$ threadDelay $ 1  -- This will let us witness nondeterminism
        --                          -- in a few hundred repetitions.
        liftIO$ threadDelay $ 0  -- This is even better.
        -- VERY BAD: this is a hole:
        lift$ V1.liftST$ write vi 0 99)
    (do vo2 <- getVecT
        V1.liftST$ do write vo2 0 120
        lift$ V1.liftST$ write vi 0 5)
    
  z <- V1.liftST$ freeze vo
  
  zz <- lift$ V1.liftST$ freeze vi 
    
  return$ show z ++ " " ++ show zz
        
t5 :: String    
t5 = LV.runPar $ runVecT $ runVecT p5
                     
--------------------------------------------------------------------------------
-- Tests for ParVec (V2)
--------------------------------------------------------------------------------

case_v2_t2 :: Assertion
case_v2_t2 = assertEqual "basic forkWithVec usage"
            "hello fromList [0.0,0.0,35.3,0.0,0.0,0.0,0.0,46.3,0.0,0.0]" v2_t2

v2_t2 :: String
v2_t2 = LV.runPar $ 
        V2.runParVec v2_p2

-- | A simple test that modifies two locations in a vector, multiple times, in parallel.
v2_p2 :: V2.ParVec s1 Float d s2 String
v2_p2 = do
  r <- V2.liftST$ newSTRef "hi"
  V2.initVec 10
  v0 <- V2.getVec

  V2.liftST$ set v0 0

  V2.forkWithVec 5
     (do v1 <- V2.getVec
         -- We can't protect against this sort of out-of-bounds error
         -- at compile time -- for that we'd need dependent types.
         -- V2.liftST$ write v1 9 0 -- BAD! out of bounds
         V2.liftST$ do write v1 2 33.3
                       tmp <- read v1 2
                       write v1 2 (tmp + 2)
     )
     (do v2 <- V2.getVec
         -- This, we actually *can* protect against at compile time.
         V2.liftST$ do write v2 2 44.3
                       tmp <- read v2 2
                       write v2 2 (tmp + 2)         
     )

  -- After the barrier we can access v0 again:
  z <- V2.liftST$ freeze v0

  V2.liftST$ writeSTRef r "hello "
  hello <- V2.liftST$ readSTRef r
  return$ hello ++ show z

-- | Use IVars mixed with VecPar:
v2_p3 :: V2.ParVec s1 Float d s2 String 
v2_p3 = do  
  V2.initVec 10
  v0 <- V2.getVec
  let st = V2.liftST
      pr = V2.liftPar
  st$ set v0 10
  iv <- V2.liftPar IV.new
  V2.forkWithVec 5
     (do v1 <- V2.getVec
         st$ do write v1 2 33.3
         tmp <- st$ read v1 2
         pr$ IV.put iv tmp                       
         st$ write v1 2 (tmp + 2))
     (do v2 <- V2.getVec
         st$ write v2 2 44.3
         tmp <- st$ read v2 2
         inp <- pr$ IV.get iv
         st$ write v2 2 (tmp + inp))
  -- After the barrier we can access v0 again:
  z   <- st$ freeze v0
  val <- pr$ IV.get iv
  return$ show (val, z)
  
v2_t3 :: String
v2_t3 = LV.runPar $
        V2.runParVec v2_p3
     
case_v2_t3 :: Assertion
case_v2_t3 = assertEqual "stacked Vec, ST, and Par effects"
          "(33.3,fromList [10.0,10.0,35.3,10.0,10.0,10.0,10.0,77.6,10.0,10.0])"
          v2_t3

-- -- | Given a vector of "unknown" length, find the length.
-- printLength :: VecT s Float (LV.Par d s2) String
-- printLength = do
--   initVecT 120
--   v <- getVecT
--   let len = (length v)
--   return$ show len

-- t4 :: String
-- t4 = LV.runPar $ runVecT printLength

-- case_t4 :: Assertion
-- case_t4 = assertEqual "test fetching a vector length"
--           "120" t4
       
-- -- | Try to forkWithVec on a stack of VecT
-- -- 
-- -- BUG: This could output two different results depending on data races.
-- p5 :: VecT s2 Int (VecT s1 Int (LV.Par d s0)) String
-- p5 = do
--   initVecT 10
--   vo <- getVecT
--   V1.liftST$ set vo 0
  
--   lift$ initVecT 10
--   vi <- lift$ getVecT
--   lift$ V1.liftST$ set vi 1
  
--   forkWithVec 5
--     (do vo1 <- getVecT
--         V1.liftST$ do write vo1 0 5
--         -- liftIO$ threadDelay $ 1  -- This will let us witness nondeterminism
--         --                          -- in a few hundred repetitions.
--         liftIO$ threadDelay $ 0  -- This is even better.
--         -- VERY BAD: this is a hole:
--         lift$ V1.liftST$ write vi 0 99)
--     (do vo2 <- getVecT
--         V1.liftST$ do write vo2 0 120
--         lift$ V1.liftST$ write vi 0 5)
    
--   z <- V1.liftST$ freeze vo
  
--   zz <- lift$ V1.liftST$ freeze vi 
    
--   return$ show z ++ " " ++ show zz
        
-- t5 :: String    
-- t5 = LV.runPar $ runVecT $ runVecT p5

----------------------------------------------------------------------------------------------------

{-

-- Takes a stack of two VecTs as we have to worry about both the array
-- and the secondary buffer array. s1 is going to be the "real" array,
-- and when calling the mergeSort wrapper a "buffer" array s2 will be
-- layered on.
mergeSort :: VecT s2 Int (VecT s1 Int (LV.Par d s0)) ()
mergeSort = do
  vi <- lift$ getVecT
  vo <- getVecT
  
  if length vi <= 1 then
    return ()
   else do
    forkWithVec 

-}

{-

mergeSort :: VecT s Int (LV.Par d s2) ()
mergeSort = do
  vec <- getVecT
  let len = length vec
  
  if len > 1 then do
    forkWithVec (len / 2)
      mergeSort
      mergeSort
    z <- liftST$ freeze vec
    return$ show z          
           
msd :: String

msd = LV.runPar $ runVecT $ do
  initVecT 4
  v <- getVecT
  liftST$ do
    write v 0 137
    write v 1 5
    write v 2 120
    write v 3 42
  mergeSort
--}

main = $(defaultMainGenerator)
-- main = do putStrLn t5
