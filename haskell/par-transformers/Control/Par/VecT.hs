{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}

-- | This file provides a basic capability for parallel in-place modification of
-- (disjoint) partitions of an array.  It allows a `Par` computation to carry an
-- implicit vector in the background, which allows mutation via arbitrary `ST`
-- computations.

module Control.Par.VecT
       (
         -- * The monad transformer: a dischargable effect
         VecT, runVecT,

         -- * An alternate fork operation 
         forkWithVec,

         -- * Accessing the threaded Vector state
         getVecT, initVecT,
         
         -- * Working with ST
         liftST, dropST
       )
       where

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
-- import qualified Control.LVish as LV
-- import qualified Data.LVar.IVar as IV -- ParFuture/ParIVar Instances.
-- import Data.LVar.IVar ()

--------------------------------------------------------------------------------

-- | The ParVec monad.  It uses the StateT monad transformer to layer
-- a state of type (STVector s elt) on top of an inner monad, ParIO.
-- Its third parameter, 'a', is the type inside the ParVec
-- computation, and the 'elt' parameter is the element type of the
-- vector.  The 's' parameter is what's known as a "phantom type".

newtype VecT s elt par a = VecT ((S.StateT (STVector s elt) par) a)

-- newtype VecT s elt a = VecT ((S.StateT (STVector s elt) ParIO) a)
 deriving (Monad, Functor)

-- | @runVecT@ discharges the extra state effect leaving the the underlying par
-- Computation -- just like `runStateT`.  Here, using the standard trick runVecT has
-- a rank-2 type, with a phantom type 's'
runVecT :: forall a elt par . PC.ParFuture par => 
           (forall s . VecT s elt par a) -> par a
-- Here we're just using the VecT value constructor tag to
-- destructure the argument to runVecT.  The 'st' in (VecT st) is
-- of the type ((S.StateT (STVector s elt) ParIO) a).  The
-- unsafePerformIO lets us get the needed 'a' out of an IO
-- computation.
runVecT (VecT st) = unsafePerformIO io
 where
   -- Create a new mutable vector of length 0 and do a runStateT with
   -- it, getting back a monadic value of type ParIO, which we then
   -- run, getting a value and a final state.  We keep the value and
   -- throw away the state.
   io = do vec <- MV.new 0 -- :: IO (STVector RealWorld elt)
           let xm :: par (a, STVector RealWorld elt)
               xm = S.runStateT st vec
               xm' = xm >>= (return . fst)
           return xm'
           
-- | getVecT is a VecT computation that results in the current
-- value of the state, which is of type 'STVector s elt'.
getVecT :: PC.ParFuture par => VecT s elt par (STVector s elt)
getVecT = VecT S.get

-- | initVecT creates a new mutable vector and returns a VecT
-- computation with that new mutable vector's state as its state.
initVecT :: PC.ParFuture par =>
            Int -> VecT s elt par ()
initVecT size = do
  vec <- liftST $ MV.new size
  VecT $ S.put vec


-- | @forkWithVec@ takes a split point and two VecT computations.  It
-- gets the state of the current computation, which is a vector, and
-- then divides up that state between the two other computations.
-- Writes to those two computations actually mutate the original
-- vector.
--
-- @forkWithVec@ is a fork-join construct, rather than a one-sided fork such as
-- `fork`.
forkWithVec :: forall elt a b s par .
               (PC.ParFuture par, PC.FutContents par a) =>
               Int
            -> (forall sl . VecT sl elt par a) -- ^ Left child computation.
            -> (forall sr . VecT sr elt par b) -- ^ Right child computation.
            -> VecT s elt par (a,b)
forkWithVec mid (VecT lef) (VecT rig) = VecT $ do
  vec <- S.get
  let lvec = slice 0 mid vec
      rvec = slice mid (length vec - mid) vec
  lv <- lift$ PC.spawn_$ S.evalStateT lef lvec
  S.put rvec
  rx <- rig                     -- Do the R one on this thread.
  lx <- lift$ PC.get lv         -- Wait for the forked thread to finish.
  S.put vec                     -- Put the whole vec back in place.
  return (lx,rx)

-- | Allow `ST` computations inside `VecT` computations.
--   This operation has some overhead. 
liftST :: PC.ParFuture par =>
          ST s a -> VecT s elt par a
liftST st =
  seq thunk (return thunk)
 where
   -- WARNING: this requires locking on EACH unsafePerformIO.
   -- This is inefficient IF the underlying 'par' actually would
   -- have the capability to peform IO more efficiently.
   -- But we can't assume that.
   thunk = unsafePerformIO io
   io    = unsafeSTToIO st 

-- | Rather than lifting ST into the VecT, drop it into the underlying `par` monad as IO.
--   In some situations this might be more efficient.   
dropST :: (MonadIO par) =>
          ST s a -> VecT s elt par a
dropST = VecT . liftIO . unsafeSTToIO  

instance PC.ParIVar par =>
-- instance PC.ParFuture par =>
         PC.ParFuture (VecT s elt par) where
  type Future      (VecT s elt par)   = PC.Future par
  type FutContents (VecT s elt par) a = PC.FutContents par a
  spawn_ (VecT task) = VecT $ 
    do iv <- lift $ PC.new
       lift $ PC.fork $ do
           (res,_) <- S.runStateT task
                      (error "spawn_: This child thread does not have permission to touch the array!")
           PC.put_ iv res
       return iv
  get iv = VecT $ lift $ PC.get iv


instance PC.ParIVar par =>
         PC.ParIVar (VecT s elt par) where
  fork (VecT task) = VecT $ 
    lift $ PC.fork $ do
      (res,_) <- S.runStateT task
                 (error "fork: This child thread does not have permission to touch the array!")
      return res
  new       = VecT$ lift PC.new
  put_ iv v = VecT$ lift$ PC.put_ iv v
  

--------------------------------------------------------------------------------
-- Tests and Scrap:
--------------------------------------------------------------------------------

-- Little tests:
t1 :: IO String
t1 = unsafeSTToIO p1

p1 :: ST s String
p1 = do
  r <- newSTRef "hi"
  writeSTRef r "hello"
  readSTRef r

