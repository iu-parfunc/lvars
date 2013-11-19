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

-- |
--  This file provides a basic capability for parallel in-place modification of
-- (disjoint) partitions of an array.  It allows a `Par` computation to carry an
-- implicit vector in the background, which allows mutation via arbitrary `ST`
-- computations.
--
-- This module does NOT provide a monad-transformer.  Rather, it is a ROOT for a
-- monad-transformer stack, and is hard-wired to use the "Control.LVish" version of
-- the `Par` monad underneath.

module Control.Par.ST
       (
         -- * The monad: a dischargable effect
         ParST, runParST,

         -- * An alternate fork operation 
         forkSTSplit,

         -- * Working with ST and other lifts
         liftST, liftPar,
         
         -- * Convert between state types
         transmute,

         --  Useful utilities
--         vecParMap_, 
         
         -- * Type class for valid states.
         STSplittable(..),
         
         -- * Annoying newtypes and wrappers to take the @s@ param last:
         MVectorFlp(..), UVectorFlp(..), SVectorFlp(..),
         STTup2(..), STUnit(..)
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
import qualified Data.Vector.Storable.Mutable as MS
import Data.Vector       (freeze)
import Prelude hiding (read, length)
import System.IO.Unsafe (unsafePerformIO)

import GHC.Prim (RealWorld)

import qualified Control.Par.Class as PC
import Control.Par.Class.Unsafe (ParThreadSafe(unsafeParIO), ParMonad(..))

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

-- | A splittable type which contains no information.
data STUnit s = STUnit
-- newtype STUnit s = STUnit ()
  

instance STSplittable STUnit where
  type SplitIdx STUnit = ()
  splitST () STUnit = (STUnit,STUnit)

------------------------------------------------------------

-- | An annoying type alias simply for the purpose of arranging for the 's' parameter
-- to be last.  Also carries the `Unbox` constraint.
data UVectorFlp a s = (MU.Unbox a) => UFlp (MU.MVector s a)  

instance STSplittable (UVectorFlp a) where
  type SplitIdx (UVectorFlp a) = Int
  splitST mid (UFlp vec) = 
    let lvec = MU.slice 0 mid vec
        rvec = MU.slice mid (MU.length vec - mid) vec
    in (UFlp lvec, UFlp rvec)

-- | An annoying type alias simply for the purpose of arranging for the 's' parameter
-- to be last.  Also carries the `Storable` constraint.
data SVectorFlp a s = (MS.Storable a) => SFlp (MS.MVector s a)

instance STSplittable (SVectorFlp a) where
  type SplitIdx (SVectorFlp a) = Int
  splitST mid (SFlp vec) = 
    let lvec = MS.slice 0 mid vec
        rvec = MS.slice mid (MS.length vec - mid) vec
    in (SFlp lvec, SFlp rvec)

--------------------------------------------------------------------------------

-- | The ParST monad.  It uses the StateT monad transformer to layer
-- a state of type on top of an inner monad, `Control.LVish.Par`.
--
-- The first type parameter determines the type of state held.  The parameters `det`
-- and `s2` are for the underlying `Par` monad.
-- 
-- Its final parameter, 'ans', is the result of running the entire computation, after
-- which the vector is no longer accessible.
newtype ParST stState parM ans =
        ParST ((S.StateT stState parM) ans)
 deriving (Monad, Functor)

-- | @runParST@ discharges the extra state effect leaving the the underlying `Par`
-- computation only -- just like `runStateT`.  Here, using the standard trick
-- runParST has a rank-2 type, with a phantom type @s1@.
runParST :: forall stt s0 parM ans . (ParThreadSafe parM) => 
             stt s0
             -> (forall s1 . ParST (stt s1) parM ans)
             -> parM ans
runParST initVal (ParST st) = do
  let xm :: parM (ans, stt s0)
      xm = S.runStateT st initVal
  xm >>= (return . fst)

-- | We use the generic interface for `put` and `get` on the entire (mutable) state.
instance ParThreadSafe parM =>
         S.MonadState stts (ParST stts parM) where
  get = reify
  put = install

-- | A `ParST` computation that results in the current value of the state, which is
-- typically some combination of `STRef` and `STVector`s.  These require `ST`
-- computation to do anything with the state.
reify :: ParThreadSafe parM => ParST stt parM stt
reify = ParST S.get

-- | Installs a new piece of ST-mutable state.
install :: ParThreadSafe parM => stt -> ParST stt parM ()
install val = ParST (S.put val)

-- | @forkWithVec@ takes a split point and two ParST computations.  It
-- gets the state of the current computation, for example a vector, and
-- then divides up that state between the two other computations.
--
-- Writes in those two computations may actually mutate the original vector.  But
-- @forkWithVec@ is a fork-join construct, rather than a one-sided fork such as
-- `fork`.  So the continuation of @forkWithVec@ will not run until both child
-- computations return, and are thus done accessing the state.
forkSTSplit :: forall a b s0 parM stt.
               (ParThreadSafe parM, PC.ParFuture parM, PC.FutContents parM a,
                Eq a, STSplittable stt) =>
               (SplitIdx stt)                      -- ^ Where to split the data.
            -> (forall sl . ParST (stt sl) parM a) -- ^ Left child computation.
            -> (forall sr . ParST (stt sr) parM b) -- ^ Right child computation.
            -> ParST (stt s0) parM (a,b)
forkSTSplit spltidx (ParST lef) (ParST rig) = ParST $ do
  snap <- S.get
  let slice1, slice2 :: stt s0
      (slice1,slice2) = splitST spltidx snap
  lift$ do lv <- PC.spawn_$ S.evalStateT lef slice1
           rx <- S.evalStateT rig slice2
           lx <- PC.get lv  -- Wait for the forked thread to finish.
           return (lx,rx)

-- | Allow `ST` computations inside `ParST` computations.
--   This operation has some overhead. 
liftST :: ParThreadSafe parM => ST s1 a -> ParST (stt s1) parM a
liftST st = ParST (lift (unsafeParIO io))
 where
   io = unsafeSTToIO st 

-- | Lift an ordinary `Par` computation into `ParST`.
liftPar :: ParThreadSafe parM => parM a -> ParST stt1 parM a 
liftPar m = ParST (lift m)

transmute :: forall a b s parM ans . 
             (ParThreadSafe parM, 
              STSplittable a,
              STSplittable b)              
              => (b s -> a s) -> ParST (a s) parM ans -> ParST (b s) parM ans
transmute fn (ParST comp) = ParST$ do
  orig <- S.get
  let newSt :: a s
      newSt = fn orig
  (res,_) <- lift$ S.runStateT comp newSt
  return $! res
  
instance ParMonad parM => ParMonad (ParST stt1 parM) where
  internalLiftIO io = ParST (lift (internalLiftIO io))
  fork (ParST task) = ParST $ 
    lift $ PC.fork $ do
      (res,_) <- S.runStateT task
                 (error "fork: This child thread does not have permission to touch the array!")
      return res

-- | A conditional instance which will only be usable if unsafe imports are made.
-- instance MonadIO parM => MonadIO (ParST stt1 parM) where
--   liftIO io = ParST (liftIO io)

-- | An instance of `ParFuture` for @ParST@ _does_ let us do arbitrary `fork`s at the
-- @ParST@ level, HOWEVER the state is inaccessible from within these child computations.
instance PC.ParFuture parM => PC.ParFuture (ParST sttt parM) where
  -- | The `Future` type and `FutContents` constraint are the same as the underlying `Par` monad.
  type Future      (ParST sttt parM)   = PC.Future      parM
  type FutContents (ParST sttt parM) a = PC.FutContents parM a
  spawn_ (ParST task) = ParST $
     lift $ PC.spawn_ $ do
           (res,_) <- S.runStateT task
                      (error "spawn_: This child thread does not have permission to touch the array!")
           return res     
  get iv = ParST $ lift $ PC.get iv


instance PC.ParIVar parM => PC.ParIVar (ParST sttt parM) where
  new       = ParST$ lift PC.new
  put_ iv v = ParST$ lift$ PC.put_ iv v

--------------------------------------------------------------------------------


-- | Generic way to build an in-place map operation for a collection state.
--
--   This function reserves the right to sequentialize some iterations.
mkParMapM :: forall elt s1 stt parM .
             (STSplittable stt, ParThreadSafe parM,
              PC.ParFuture parM, PC.FutContents parM ()) =>
              (forall s4 . Int ->        ParST (stt s4) parM elt) -- ^ Reader
           -> (forall s4 . Int -> elt -> ParST (stt s4) parM ())  -- ^ Writer 
           -> (forall s4 .               ParST (stt s4) parM Int) -- ^ Length
           -> (Int -> SplitIdx stt)                               -- ^ Split elements
           -> (elt -> parM elt)                                   -- ^ Fn to map over elmts.
           -> ParST (stt s1) parM ()
{-# INLINE mkParMapM #-}
mkParMapM reader writer getsize mksplit fn = do
  stt <- S.get
  len <- getsize 
  let share = max 1 (len `quot` (numProcs * overPartition))
      loopmpm :: Int -> (forall s3 . ParST (stt s3) parM ())
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


-- | A simple for loop for numeric ranges (not requiring deforestation
-- optimizations like `forM`).  Inclusive start, exclusive end.
{-# INLINE for_ #-}
for_ :: Monad m => (Int, Int) -> (Int -> m ()) -> m ()
for_ (start, end) _fn | start > end = error "for_: start is greater than end"
for_ (start, end) fn = loop start
 where
  loop !i | i == end  = return ()
          | otherwise = do fn i; loop (i+1)
