{-# LANGUAGE BangPatterns, ConstraintKinds, DataKinds, FlexibleContexts,
             FlexibleInstances, GADTs, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, Rank2Types, ScopedTypeVariables,
             TupleSections, TypeFamilies, TypeSynonymInstances #-}

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
       {-(
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
       )-}
       where

import Control.Applicative
import Control.Monad

-- Transformers:
-- import qualified Control.Monad.Trans as T
-- import qualified Control.Monad.Trans.State.Strict as S

-- mtl:
import qualified Control.Monad.State.Strict as S
-- import qualified Control.Monad.State.Class (MonadState(..))

import Control.Monad.ST (ST)
import Control.Monad.ST.Unsafe (unsafeIOToST, unsafeSTToIO)
import Control.Monad.Trans (lift)
import Control.Par.EffectSigs

import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Storable.Mutable as MS
import qualified Data.Vector.Unboxed.Mutable as MU
import Prelude hiding (length, read)

import qualified Control.Par.Class as PC
import Control.Par.Class.Unsafe (ParMonad (..), ParThreadSafe (unsafeParIO))

import GHC.Conc (getNumProcessors)
import System.IO.Unsafe (unsafeDupablePerformIO)

{-# INLINE unsafeCastST #-}
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
newtype VecSplit = SplitAt Int

-- | An annoying type alias simply for the purpose of arranging for the 's' parameter
-- to be last.
newtype MVectorFlp a s = VFlp (MV.MVector s a)

instance STSplittable (MVectorFlp a) where
  type SplitIdx (MVectorFlp a) = Int
  {-# INLINE splitST #-}
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
  {-# INLINE splitST #-}
  splitST (spltA,spltB) (STTup2 a b) =
    let (a',a'') = splitST spltA a
        (b',b'') = splitST spltB b
    in ((STTup2 a' b'), (STTup2 a'' b''))

-- | A splittable type which contains no information.
data STUnit s = STUnit
-- newtype STUnit s = STUnit ()


instance STSplittable STUnit where
  type SplitIdx STUnit = ()
  {-# INLINE splitST #-}
  splitST () STUnit = (STUnit,STUnit)

------------------------------------------------------------

-- | An annoying type alias simply for the purpose of arranging for the 's' parameter
-- to be last.  Also carries the `Unbox` constraint.
data UVectorFlp a s = (MU.Unbox a) => UFlp (MU.MVector s a)

instance STSplittable (UVectorFlp a) where
  type SplitIdx (UVectorFlp a) = Int
  {-# INLINE splitST #-}
  splitST mid (UFlp vec) =
    let lvec = MU.slice 0 mid vec
        rvec = MU.slice mid (MU.length vec - mid) vec
    in (UFlp lvec, UFlp rvec)

-- | An annoying type alias simply for the purpose of arranging for the 's' parameter
-- to be last.  Also carries the `Storable` constraint.
data SVectorFlp a s = (MS.Storable a) => SFlp (MS.MVector s a)

instance STSplittable (SVectorFlp a) where
  type SplitIdx (SVectorFlp a) = Int
  {-# INLINE splitST #-}
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
newtype ParST stState (p :: EffectSig -> * -> * -> *) e s a =
        ParST (stState -> p e s (a,stState))

data ParStateT (state :: *) (p :: EffectSig -> * -> * -> *) e s a =
  ParStateT (state -> p e s (a,state))

-- | @runParST@ discharges the extra state effect leaving the the underlying `Par`
-- computation only -- just like `runStateT`.  Here, using the standard trick
-- runParST has a rank-2 type, with a phantom type @s1@.
--
-- `stt` is the type constructor for the state kept in the monad, e.g. `MVectorFlp`.
--
{-# INLINE runParST #-}
runParST :: forall stt s0 (parM :: EffectSig -> * -> * -> *) ef s2 ans .
            (ParThreadSafe parM, Monad (parM ef s2)) =>
             stt s0
             -> (forall s1 . ParST (stt s1) parM ef s2 ans)
             -> parM ef s2 ans
runParST initVal (ParST fn) = do
  let xm :: parM ef s2 (ans, stt s0)
      xm = fn initVal
  xm >>= (return . fst)

-- | A `ParST` computation that results in the current value of the state, which is
-- typically some combination of `STRef` and `STVector`s.  These require `ST`
-- computation to do anything with the state.
{-# INLINE reify #-}
reify :: (Monad (p e s), ParThreadSafe p) => ParST stt p e s stt
reify = ParST $ \s -> return (s, s)

-- | Installs a new piece of ST-mutable state.
--
-- DEPRECATED: This is unsafe because it doesn't enforce alias freedom.
{-# INLINE install #-}
install :: (Monad (p e s), ParThreadSafe p) => stt -> ParST stt p e s ()
install val = ParST $ \_ -> return ((), val)

instance ParMonad parM => ParMonad (ParST stt1 parM) where
  {-# INLINE internalLiftIO #-}
  internalLiftIO io = liftPar (internalLiftIO io)
  {-# INLINE fork #-}
  fork (ParST task) = liftPar $ PC.fork $ do
      (res,_) <- task
                 (error "fork: This child thread does not have permission to touch the array!")
      return res

-- | Lift an ordinary `Par` computation into `ParST`.
{-# INLINE liftPar #-}
liftPar :: ParMonad parM => parM e s a -> (ParST stt1 parM) e s a
liftPar m = ParST (\s -> m `pbind` (\x -> preturn (x,s)))

-- | We use the generic interface for `put` and `get` on the entire (mutable) state.
instance (ParMonad p, Monad (p e s), ParThreadSafe p) =>
         S.MonadState stts (ParST stts p e s) where
  {-# INLINE get #-}
  get = reify
  {-# INLINE put #-}
  put = install

-- | Allow `ST` computations inside `ParST` computations.
--   This operation has some overhead.
{-# INLINE liftST #-}
liftST :: (ParMonad p, ParThreadSafe p) => ST s a -> ParST (stt s1) p e s a
liftST st = ParST $ \s -> do r <- unsafeParIO io; return (r, s)
 where
   io = unsafeSTToIO st

{-# INLINE overPartition #-}
overPartition :: Int
overPartition = 8

{-# INLINE numProcs #-}
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

{-# INLINE transmute #-}
-- transmute :: forall a b s p e ans .
--              (ParMonad p,
--               ParThreadSafe p,
--               STSplittable a,
--               STSplittable b)
--               => (b s -> a s) -> ParST (a s) p e s ans -> ParST (b s) p e s ans
transmute fn (ParST comp) = do
  orig <- S.get
  let -- newSt :: a s
      newSt = fn orig
  (res, _) <- lift $ comp newSt
  return $! res

-- | @forkSTSplit@ takes a split point and two ParST computations.  It gets the
-- state of the current computation, for example a vector, and then divides up
-- that state between the two other computations.
--
-- Writes in those two computations may actually mutate the original data
-- structure.  But @forkSTSplit@ is a fork-join construct, rather than a
-- one-sided fork such as `fork`.  So the continuation of @forkSTSplit@ will not
-- run until both child computations return, and are thus done accessing the
-- state.
{-# INLINE forkSTSplit #-}
forkSTSplit
  :: forall p t stt sFull e s.
     (PC.FutContents p (t, stt sFull), PC.ParFuture p, STSplittable stt,
      GetP e ~ 'P, GetG e ~ 'G)
     => SplitIdx stt                        -- ^ Where to split the data.
     -> (forall sl. ParST (stt sl) p e s t) -- ^ Left child computation.
     -> (forall sr. ParST (stt sr) p e s t) -- ^ Right child computation.
     -> ParST (stt sFull) p e s (t, t)
forkSTSplit spltidx (ParST lef) (ParST rig) = ParST $ \snap -> do
  let slice1, slice2 :: stt sFull
      (slice1, slice2) = splitST spltidx snap
  lv <- PC.spawn_ $ lef slice1
  (rx, _) <- rig slice2
  (lx, _) <- PC.get lv
  return ((lx, rx), snap) -- FIXME: Should we ignore modified states?

-- | A conditional instance which will only be usable if unsafe imports are made.
-- FIXME: Not conditional right now.
-- FIXME: This won't work. We may need our own ParMonadIO typeclass.
-- instance MonadIO p => MonadIO (ParST s p e s) where
--   liftIO io = ParST (liftIO io)

-- | An instance of `ParFuture` for @ParST@ _does_ let us do arbitrary `fork`s at the
-- @ParST@ level, HOWEVER the state is inaccessible from within these child computations.
instance PC.ParFuture parM => PC.ParFuture (ParST sttt parM) where
  -- | The `Future` type and `FutContents` constraint are the same as the
  -- underlying `Par` monad.
  type Future      (ParST sttt parM)   = PC.Future      parM
  type FutContents (ParST sttt parM) a = PC.FutContents parM a

  {-# INLINE spawn_ #-}
  spawn_ (ParST task) = ParST $ \st -> -- TODO: Why can't I use `return` here?
     fmap (,st) $ PC.spawn_ $ do
       (res, _) <- task $
         error "spawn_: This child thread does not have permission to touch the array!"
       return res

  {-# INLINE get #-}
  get iv = ParST $ \st -> (,st) <$> PC.get iv

instance PC.ParIVar parM => PC.ParIVar (ParST sttt parM) where
  {-# INLINE new #-}
  new       = ParST $ \st -> (,st) <$> PC.new
  {-# INLINE put_ #-}
  put_ iv v = ParST $ \st -> (,st) <$> PC.put_ iv v

{-

--------------------------------------------------------------------------------
-- | Generic way to build an in-place map operation for a collection state.
--
--   This function reserves the right to sequentialize some iterations.
mkParMapM :: forall elt s1 stt p e s .
             (STSplittable stt, ParThreadSafe p,
              PC.ParFuture p, PC.FutContents p (), GetP e ~ 'P, GetG e ~ 'G) =>
              (forall s2 . Int ->        ParST (stt s2) p e s elt) -- ^ Reader
           -> (forall s2 . Int -> elt -> ParST (stt s2) p e s ())  -- ^ Writer
           -> (forall s2 .               ParST (stt s2) p e s Int) -- ^ Length
           -> (Int -> SplitIdx stt)                                -- ^ Split elements
           -> (elt -> p e s elt)                                   -- ^ Fn to map over elmts.
           -> ParST (stt s1) p e s ()
{-# INLINE mkParMapM #-}
mkParMapM reader writer getsize mksplit fn = do
  stt <- S.get
  len <- getsize
  let share = max 1 (len `quot` (numProcs * overPartition))
      loopmpm :: Int -> (forall ls . ParST (stt ls) p e s ())
      loopmpm iters
        | iters <= share =
          -- Bottom out to a sequential loop:
          for_ (0,iters) $ \ ind -> do
            x <- reader ind
            y <- liftPar $ fn x
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

-}
