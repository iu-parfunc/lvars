{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.LVish      as LV
import Data.LVar.IVar () -- Instances.
import Control.Par.VecT   as V
import Control.Par.StateT as S

import Control.Monad
import Control.Monad.IO.Class
import qualified Control.Monad.Trans.State.Strict as S
import Control.Monad.ST        (ST)
import Control.Monad.ST.Unsafe (unsafeSTToIO)
import Control.Monad.Trans (lift)

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
  r <- liftST$ newSTRef "hi"
  initVecT 10
  v0 <- getVecT

  liftST$ set v0 0

  forkWithVec 5
     (do v1 <- getVecT
         -- We can't protect against this sort of out-of-bounds error
         -- at compile time -- for that we'd need dependent types.
         -- liftST$ write v1 9 0 -- BAD! out of bounds
         liftST$ do write v1 2 33.3
                    tmp <- read v1 2
                    write v1 2 (tmp + 2)
     )
     (do v2 <- getVecT
         -- This, we actually *can* protect against at compile time.
         -- liftST$ read v 2  -- BAD!
         -- liftST$ readSTRef r
         liftST$ do write v2 2 44.3
                    tmp <- read v2 2
                    write v2 2 (tmp + 2)         
     )

  -- After the barrier we can access v0 again:
  z <- liftST$ freeze v0

  liftST$ writeSTRef r "hello "
  hello <- liftST$ readSTRef r
  return$ hello ++ show z

{--

-- | Given a vector of "unknown" length, find the length.
printLength :: VecT s Float (LV.Par d s2) String
printLength = do
  initVecT 120
  v <- getVecT
  let len = (length v)
  return$ show len

tpr :: String
tpr = LV.runPar $ runVecT printLength


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

