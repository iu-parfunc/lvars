{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}


{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE GADTs #-}

-- |
--  This file provides a basic capability for parallel in-place modification of
-- (disjoint) partitions of an array.  It allows a `Par` computation to carry an
-- implicit vector in the background, which allows mutation via arbitrary `ST`
-- computations.
--
-- This module does NOT provide a monad-transformer.  Rather, it is a ROOT for a
-- monad-transformer stack, and is hard-wired to use the "Control.LVish" version of
-- the `Par` monad underneath.

module Control.LVish.ST
       (
         -- * The monad: a dischargable effect
         ParST, runParST,

         -- * An alternate fork operation 
         forkSTSplit,

         -- * Working with ST and other lifts
         liftST, liftPar,

         --  Useful utilities
--         vecParMap_, 
         
         -- * Type class for valid states.
         STSplittable(..),
         
         -- * Annoying newtypes and wrappers to take the @s@ param last:
         MVectorFlp(..), UVectorFlp(..), STTup2(..)
       )
       where

import Control.Monad
import Control.Monad.IO.Class

-- Transformers:
-- import qualified Control.Monad.Trans as T
-- import qualified Control.Monad.Trans.State.Strict as S

-- mtl:
import qualified Control.Monad.State.Strict as S
-- import qualified Control.Monad.State.Class (MonadState(..))

import Control.Monad.ST        (ST)
import Control.Monad.ST.Unsafe (unsafeSTToIO, unsafeIOToST)
import Control.Monad.Trans (lift)

import Data.STRef
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Vector       (freeze)
import Prelude hiding (read, length)
import System.IO.Unsafe (unsafePerformIO)

import GHC.Prim (RealWorld)

import Control.LVish (Par, for_)
import qualified Control.LVish.Internal as LI
import qualified Control.Par.Class as PC
import qualified Data.LVar.IVar as IV -- ParFuture/ParIVar Instances.

import GHC.Conc (getNumProcessors)
import System.IO.Unsafe (unsafeDupablePerformIO)

unsafeCastST :: ST s1 a -> ST s2 a
unsafeCastST = unsafeIOToST . unsafeSTToIO
  
--------------------------------------------------------------------------------

-- | The class of types that can be modified in ST computations, and whose state can
-- be partitioned into disjoint pieces to be passed linearly to exactly one parallel
-- subcomputation.
class STSplittable (ty :: * -> *) where
  -- | Something of type `SplitIdx` describes where and how to split the data into two pieces.
  type SplitIdx ty :: *
  -- | `splitST` does the actual splitting.
  splitST :: SplitIdx ty -> ty s -> (ty s, ty s)

-- | The ways to split a vector.  For now we only allow splitting into two pieces at a
-- given index.  In the future, other ways of partitioning the set of elements may be
-- possible.
-- newtype VecSplit = SplitAt Int

-- | An annoying type alias simply for the purpose of arranging for the 's' parameter
-- to be last.
newtype MVectorFlp a s = VFlp (MV.MVector s a)

instance STSplittable (MVectorFlp a) where
  type SplitIdx (MVectorFlp a) = Int
  splitST mid (VFlp vec) = 
    let lvec = MV.slice 0 mid vec
        rvec = MV.slice mid (MV.length vec - mid) vec
    in (VFlp lvec, VFlp rvec)

------------------------------------------------------------

-- | An annoying type wrapper simply for the purpose of arranging for the 's' parameter
-- to be last.  
data STTup2 (a :: * -> *) (b :: * -> *) (s :: *) =
     STTup2 !(a s) !(b s)
-- I haven't figured out how to get this to work with raw, naked tuples yet.  So for
-- now, `STTup2`.

instance (STSplittable a, STSplittable b) => STSplittable (STTup2 a b) where
  type SplitIdx (STTup2 a b) = (SplitIdx a, SplitIdx b)
  splitST (spltA,spltB) (STTup2 a b) = 
    let (a',a'') = splitST spltA a 
        (b',b'') = splitST spltB b
    in ((STTup2 a' b'), (STTup2 a'' b''))

------------------------------------------------------------

-- | An annoying type alias simply for the purpose of arranging for the 's' parameter
-- to be last.
data UVectorFlp a s = (MU.Unbox a) => UFlp (MU.MVector s a)  

instance STSplittable (UVectorFlp a) where
  type SplitIdx (UVectorFlp a) = Int
  splitST mid (UFlp vec) = 
    let lvec = MU.slice 0 mid vec
        rvec = MU.slice mid (MU.length vec - mid) vec
    in (UFlp lvec, UFlp rvec)

--------------------------------------------------------------------------------

-- | The ParST monad.  It uses the StateT monad transformer to layer
-- a state of type on top of an inner monad, `Control.LVish.Par`.
--
-- The first type parameter determines the type of state held.  The parameters `det`
-- and `s2` are for the underlying `Par` monad.
-- 
-- Its final parameter, 'ans', is the result of running the entire computation, after
-- which the vector is no longer accessible.
newtype ParST stState det s2 ans =
        ParST ((S.StateT stState (Par det s2)) ans)
 deriving (Monad, Functor)

-- | @runParST@ discharges the extra state effect leaving the the underlying `Par`
-- computation only -- just like `runStateT`.  Here, using the standard trick
-- runParST has a rank-2 type, with a phantom type @s1@.
runParST :: forall stt s0 s2 det ans .
             stt s0
             -> (forall s1 . ParST (stt s1) det s2 ans)
             -> Par det s2 ans
runParST initVal (ParST st) = unsafePerformIO io
 where
   io = do 
           let xm :: Par det s2 (ans, stt s0)
               xm = S.runStateT st initVal
               xm' = xm >>= (return . fst)
           return xm'

-- | We use the generic interface for `put` and `get` on the entire (mutable) state.
instance S.MonadState stts (ParST stts det s2) where
  get = reify
  put = install
  
-- | A `ParST` computation that results in the current value of the state, which is
-- typically some combination of `STRef` and `STVector`s.  These require `ST`
-- computation to do anything with the state.
reify :: ParST stt det s2 stt
reify = ParST S.get

-- | Installs a new piece of ST-mutable state.
install :: stt -> ParST stt det s2 ()
install val = ParST (S.put val)

-- | @forkWithVec@ takes a split point and two ParST computations.  It
-- gets the state of the current computation, for example a vector, and
-- then divides up that state between the two other computations.
--
-- Writes in those two computations may actually mutate the original vector.  But
-- @forkWithVec@ is a fork-join construct, rather than a one-sided fork such as
-- `fork`.  So the continuation of @forkWithVec@ will not run until both child
-- computations return, and are thus done accessing the state.
forkSTSplit :: forall a b s0 s2 det stt.
               (Eq a, STSplittable stt) =>
               (SplitIdx stt)
            -> (forall sl . ParST (stt sl) det s2 a) -- ^ Left child computation.
            -> (forall sr . ParST (stt sr) det s2 b) -- ^ Right child computation.
            -> ParST (stt s0) det s2 (a,b)
forkSTSplit spltidx (ParST lef) (ParST rig) = ParST $ do
  snap <- S.get
  let slice1, slice2 :: stt s0
      (slice1,slice2) = splitST spltidx snap
  lift$ do lv <- IV.spawn_$ S.evalStateT lef slice1
           rx <- S.evalStateT rig slice2
           lx <- IV.get lv         -- Wait for the forked thread to finish.
           return (lx,rx)

-- | Allow `ST` computations inside `ParST` computations.
--   This operation has some overhead. 
liftST :: ST s1 a -> ParST (stt s1) det s2 a
liftST st = ParST (lift (LI.liftIO io))
 where
   io    = unsafeSTToIO st 

-- | Lift an ordinary `Par` computation into `ParST`.
liftPar :: Par d s a -> ParST stt1 d s a 
liftPar m = ParST (lift m)

-- | A conditional instance which will only be usable if unsafe imports are made.
instance MonadIO (Par det s2) => MonadIO (ParST stt1 det s2) where
  liftIO io = ParST (liftIO io)

-- | An instance of `ParFuture` for @ParST@ _does_ let us do arbitrary `fork`s at the
-- @ParST@ level, HOWEVER the state is inaccessible from within these child computations.
instance PC.ParFuture (ParST sttt d s) where
  -- | The `Future` type and `FutContents` constraint are the same as the underlying `Par` monad.
  type Future      (ParST sttt d s)   = PC.Future      (Par d s)
  type FutContents (ParST sttt d s) a = PC.FutContents (Par d s) a
  spawn_ (ParST task) = ParST $ 
    do iv <- lift $ PC.new
       lift $ PC.fork $ do
           (res,_) <- S.runStateT task
                      (error "spawn_: This child thread does not have permission to touch the array!")
           PC.put_ iv res
       return iv
  get iv = ParST $ lift $ PC.get iv

instance PC.ParIVar (ParST sttt d s) where
  fork (ParST task) = ParST $ 
    lift $ PC.fork $ do
      (res,_) <- S.runStateT task
                 (error "fork: This child thread does not have permission to touch the array!")
      return res
  new       = ParST$ lift PC.new
  put_ iv v = ParST$ lift$ PC.put_ iv v

--------------------------------------------------------------------------------


-- | Generic way to build an in-place map operation for a collection state.
--
--   This function reserves the right to sequentialize some iterations.
mkParMapM :: forall elt det s1 s2 stt . (STSplittable stt) =>
           -- (Int -> ST s1 elt) ->
           -- (Int -> elt -> ST s1 ()) ->
           -- (stt s1 -> ST s1 Int) ->
              (forall s4 . Int ->        ParST (stt s4) det s2 elt)  -- ^ Reader
           -> (forall s4 . Int -> elt -> ParST (stt s4) det s2 ())   -- ^ Writer 
           -> (forall s4 .               ParST (stt s4) det s2 Int)  -- ^ Length
           -> (Int -> SplitIdx stt)                                  -- ^ Split elements
           -> (elt -> Par det s2 elt)                                -- ^ Fn to map over elmts.
           -> ParST (stt s1) det s2 ()
{-# INLINE mkParMapM #-}
mkParMapM reader writer getsize mksplit fn = do
  stt <- S.get
  len <- getsize 
  let share = max 1 (len `quot` (numProcs * overPartition))
      loopmpm :: Int -> (forall s3 . ParST (stt s3) det s2 ())
      loopmpm iters
        | iters <= share = 
          -- Bottom out to a sequential loop:
          for_ (0,iters) $ \ ind -> do  
            x <- reader ind
            y <- liftPar$ fn x
            writer ind y
            return ()
          
        | otherwise = do
            let (iters2,extra) = iters `quotRem` 2
                iters1 = iters2+extra
            forkSTSplit (mksplit iters1)
              (loopmpm iters1)
              (loopmpm iters2)
            return ()
  return ()

overPartition :: Int
overPartition = 8

numProcs :: Int
numProcs = unsafeDupablePerformIO getNumProcessors

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

