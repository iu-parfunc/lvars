{-# LANGUAGE BangPatterns, ConstraintKinds, CPP, DataKinds,
             FlexibleContexts, FlexibleInstances, GADTs,
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             Rank2Types, ScopedTypeVariables, TupleSections,
             TypeFamilies, TypeSynonymInstances, NamedFieldPuns #-}

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
         ParST, runParST, unsafeRunParST,
         reify, unsafeInstall,

         -- * An alternate fork operation
         forkSTSplit,

         -- * Working with ST and other lifts
         liftST, liftPar,

         -- * Convert between state types
         transmute,

         -- * Type class for valid states of ParST.
         STSplittable(..),

         -- * Annoying newtypes and wrappers to take the @s@ param last:
         MVectorFlp(..), UVectorFlp(..), SVectorFlp(..),
         STTup2(..), STUnit(..),

         BuildRecipe(..),

         -- * TEMPORARY: Useful utilities
         for_, mkParMapM
--         vecParMap_,
       )
       where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Control.Monad.ST (ST)
import Control.Monad.ST.Unsafe (unsafeSTToIO)
-- import qualified Control.Monad.State.Strict as S
import qualified Control.Monad.Reader as R

import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Storable.Mutable as MS
import qualified Data.Vector.Unboxed.Mutable as MU
import Prelude hiding (length, read)

import qualified Control.Par.Class as PC
import Control.Par.Class.Unsafe (ParMonad (..), ParThreadSafe (unsafeParIO))
import Control.Par.EffectSigs

import GHC.Conc (getNumProcessors)
import System.IO.Unsafe (unsafeDupablePerformIO)

--------------------------------------------------------------------------------
-- | The class of types that can be modified in ST computations, and whose state can
-- be partitioned into disjoint pieces to be passed linearly to exactly one parallel
-- subcomputation.
--
-- Instances of this class are trusted to retain alias-free invariants.
class STSplittable (ty :: * -> *) where
  -- | Something of type `SplitIdx` describes where and how to split the data into two pieces.
  type SplitIdx ty :: *
  -- | `splitST` does the actual splitting.  It is guaranteed to
  -- return non-overlapping (non-aliased) pieces of the structure.
  splitST :: SplitIdx ty -> ty s -> (ty s, ty s)

  -- | To instantiate a computation using with `ty` state, we must
  -- build a new state that is guaranteed to be alias-free.  The
  -- `BuildRecipe` is a type-specific piece of pure, immutable data,
  -- which describes how to build the structure.
  data BuildRecipe ty :: *

  -- | This is an internal/unsafe method which implements the BuildRecipe.
  instantiateRecipe :: BuildRecipe ty -> ST s (ty s)

  -- | Perform a deep-copy of the state value, ensuring that even if
  -- the original value had aliases, the resulting value does not.
  cloneState :: ty s -> ST s (ty s)


{-
-- | A valid state for a ParST computation provides various recipes
-- and combinators for creation, splitting, and "zooming" on sub-parts.
class STSplittable ty => ParSTState ty where
  -- TODO: unless we think of something else to put in here.  This
  -- should be a stand-alone type family:
  type NewRecipe ty :: *
-}

-- -- | A handful of generally useful ways to initialize a vector.
-- data ArrayRecipe a = Constant { len :: Int, fill :: a }
--                      -- ^ Constant array
--                    | Consecutive { start  :: a
--                                  , end    :: a
--                                  , stride :: a }
--                    | Random a a -- ^ Lower and upper bound, inclusive.

-- | A method of initializing a fresh array.
data ArrayRecipe s a = ArrayRecipe Int (Int -> ST s a)


-- | Descriptions of guaranteed alias-free ways to build an array.

-- | An annoying type alias simply for the purpose of arranging for the 's' parameter
-- to be last.
newtype MVectorFlp a s = VFlp { unFlp :: MV.MVector s a }

-- | A single vector cannot contain aliases.
instance STSplittable (MVectorFlp a) where
  type SplitIdx (MVectorFlp a) = Int
  {-# INLINE splitST #-}
  splitST mid (VFlp vec) =
    let lvec = MV.slice 0 mid vec
        rvec = MV.slice mid (MV.length vec - mid) vec
    in (VFlp lvec, VFlp rvec)

  -- | Because any vector is internally alias-free, we accept an arbitrary vector here.

  -- type BuildRecipe (MVectorFlp a) = ST () (MVectorFlp a ())
--   newtype BuildRecipe (MVectorFlp a) = forall s . MVectorFlpRecipe (MVectorFlp a s)
--  data BuildRecipe (MVectorFlp a) = forall s . MVectorFlpRecipe (MVectorFlp a s)

  data BuildRecipe (MVectorFlp a) = MVectorFlpRecipe (forall s . ArrayRecipe s a)

  instantiateRecipe (MVectorFlpRecipe (ArrayRecipe len fn)) =
    do v <- MV.new len
       for_ (0,len) $ \ix -> do el <- (fn ix)
                                MV.write v ix el
       return (VFlp v)
  cloneState (VFlp v) =
     do v' <- MV.clone v
        return $ VFlp v'

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

  data BuildRecipe (STTup2 a b) = STTup2Recipe (BuildRecipe a) (BuildRecipe b)

  instantiateRecipe (STTup2Recipe ra rb) =
    do a <- instantiateRecipe ra
       b <- instantiateRecipe rb
       return $ STTup2 a b

  cloneState (STTup2 a b) =
    do a' <- cloneState a
       b' <- cloneState b
       return $ STTup2 a' b'


-- | Safe lifting of a computation on state `A` to one on state `(A,B)`, where
--   the second component is ignored.
liftL :: ParMonad p =>
         (ParST (st1 s0) p) e s1 a -> (ParST ((STTup2 st1 st2) s0) p) e s1 a
liftL (ParST f1) = ParST $ \(STTup2 st1 r) ->
  do (x,l') <- (f1 st1)
     return (x, STTup2 l' r)

-- | Like `liftL`, but flipped.
liftR :: ParMonad p =>
         (ParST (st1 s0) p) e s1 a -> (ParST ((STTup2 st2 st1) s0) p) e s1 a
liftR (ParST f1) = ParST $ \(STTup2 l st1) ->
  do (x,new) <- (f1 st1)
     return (x, STTup2 l new)

-- | Swap the components of a STTup2 computation.
swap :: ParMonad p =>
      (ParST ((STTup2 st1 st2) s0) p) e s1 a
   -> (ParST ((STTup2 st2 st1) s0) p) e s1 a
swap (ParST f1) = ParST $ \(STTup2 y x) ->
  do (ans, STTup2 x' y') <- f1 (STTup2 x y)
     return (ans, STTup2 y' x')


-- | A splittable type which contains no information.
data STUnit s = STUnit
-- newtype STUnit s = STUnit ()


instance STSplittable STUnit where
  type SplitIdx STUnit = ()
  {-# INLINE splitST #-}
  splitST () STUnit = (STUnit,STUnit)

  data BuildRecipe STUnit = STUnitRecipe

  instantiateRecipe STUnitRecipe = return STUnit
  cloneState STUnit = return STUnit


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

  data BuildRecipe (UVectorFlp a) = MU.Unbox a => UVectorFlpRecipe (forall s . ArrayRecipe s a)

  instantiateRecipe (UVectorFlpRecipe (ArrayRecipe len fn)) =
    do v <- MU.new len
       for_ (0,len) $ \ix -> do el <- (fn ix)
                                MU.write v ix el
       return (UFlp v)
  cloneState (UFlp v) =
     do v' <- MU.clone v
        return $ UFlp v'

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

  data BuildRecipe (SVectorFlp a) = MS.Storable a => SVectorFlpRecipe (forall s .  ArrayRecipe s a)

  instantiateRecipe (SVectorFlpRecipe (ArrayRecipe len fn)) =
    do v <- MS.new len
       for_ (0,len) $ \ix -> do el <- (fn ix)
                                MS.write v ix el
       return (SFlp v)
  cloneState (SFlp v) =
     do v' <- MS.clone v
        return $ SFlp v'


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
        ParST { unwrapParST :: stState -> p e s (a,stState) }

-- | @runParST@ discharges the extra state effect leaving the the underlying `Par`
-- computation only -- just like `runStateT`.  Here, using the standard trick
-- runParST has a rank-2 type, with a phantom type @s1@.
--
-- `st` is the type constructor for the state kept in the monad, e.g. `MVectorFlp`.
--
-- To retain alias-freedom the user may not directly initialize the state, rather, they
-- pass a recipe to describe how to build the initial state.
{-# INLINE runParST #-}
runParST :: forall (st :: * -> *) s0 s2 (p :: EffectSig -> * -> * -> *) (e :: EffectSig) a .
            (ParMonad p, ParThreadSafe p, STSplittable st) =>
             (BuildRecipe st)
             -> (forall s1 . ParST (st s1) p e s2 a)
             -> p e s2 a
runParST recipe (ParST fn) =
  doST (instantiateRecipe recipe) `pbind` \initVal ->
   let xm :: p e s2 (a, st s0)
       xm = fn initVal
    in xm `pbind` (preturn . fst)
 where
  doST = unsafeParIO . unsafeSTToIO

-- | This version does a deep copy of the initial state.
runParST2 :: forall (st :: * -> *) s0 s2 (p :: EffectSig -> * -> * -> *) (e :: EffectSig) a .
             (ParMonad p, ParThreadSafe p, STSplittable st) =>
              st s0
              -> (forall s1 . ParST (st s1) p e s2 a)
              -> p e s2 a
runParST2 initVal (ParST fn) =
  error "runParST2"

-- | The unsafe variant allows the user to initialize with an arbitrary state.
{-# INLINE unsafeRunParST #-}
unsafeRunParST :: forall (st :: * -> *) s0 s2 (p :: EffectSig -> * -> * -> *) (e :: EffectSig) a .
                  (ParMonad p, ParThreadSafe p) =>
                   st s0
                   -> (forall s1 . ParST (st s1) p e s2 a)
                   -> p e s2 a
unsafeRunParST initVal (ParST fn) =
  let xm :: p e s2 (a, st s0)
      xm = fn initVal
   in xm `pbind` (preturn . fst)

-- | A `ParST` computation that results in the current value of the state, which is
-- typically some combination of `STRef` and `STVector`s.  These require `ST`
-- computation to do anything with the state.
{-# INLINE reify #-}
reify :: (ParMonad p, ParThreadSafe p) => ParST stt p e s stt
reify = ParST $ \s -> preturn (s, s)

-- | Installs a new piece of ST-mutable state.
--
-- DEPRECATED: This is unsafe because it doesn't enforce alias freedom.
{-# INLINE unsafeInstall #-}
unsafeInstall :: (Monad (p e s), ParThreadSafe p) => stt -> ParST stt p e s ()
unsafeInstall val = ParST $ \_ -> return ((), val)

instance ParMonad p => ParMonad (ParST st p) where
  {-# INLINE preturn #-}
  preturn a = ParST $ \st -> return (a, st)

  {-# INLINE pbind #-}
  pbind (ParST p1) f = ParST $ \st -> do
    (a, st') <- p1 st
    unwrapParST (f a) st'

  {-# INLINE internalLiftIO #-}
  internalLiftIO io = liftPar (internalLiftIO io)

  {-# INLINE fork #-}
  fork (ParST task) = liftPar $ PC.fork $ do
      (res,_) <- task
                 (error "fork: This child thread does not have permission to touch the array!")
      return res

-- | Lift an ordinary `Par` computation into `ParST`.
{-# INLINE liftPar #-}
liftPar :: ParMonad p => p e s a -> (ParST st p) e s a
liftPar m = ParST (\s -> m `pbind` (\x -> preturn (x,s)))


instance (ParMonad p, ParThreadSafe p) =>
         R.MonadReader stts (ParST stts p e s) where
  {-# INLINE ask #-}
  ask = reify

  -- local fn m =

-- | Allow `ST` computations inside `ParST` computations.
--   This operation has some overhead.
{-# INLINE liftST #-}
liftST :: (ParMonad p, ParThreadSafe p) => ST ss a -> ParST (stt ss) p e s a
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

-- | Apply a user-provided transformation function.  Warning: this is
-- unsafe because the user can destroy alias-freedom.
{-# INLINE transmute #-}
{-# DEPRECATED transmute "transmute allows violation of alias-freedom" #-}
-- transmute :: forall p e s ans a b .
--              ParMonad p =>
--              (b s -> a s) -> (ParST (a s) p) e s ans -> (ParST (b s) p) e s ans
transmute fn (ParST comp) = ParST $ \orig -> do
  (res, _) <- comp (fn orig)
  return $! (res, orig)

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
     (PC.ParIVar p, STSplittable stt,
      HasPut e, HasGet e)
     => SplitIdx stt                        -- ^ Where to split the data.
     -> (forall sl. ParST (stt sl) p e s t) -- ^ Left child computation.
     -> (forall sr. ParST (stt sr) p e s t) -- ^ Right child computation.
     -> ParST (stt sFull) p e s (t, t)
forkSTSplit spltidx (ParST lef) (ParST rig) = ParST $ \snap -> do
  let slice1, slice2 :: stt sFull
      (slice1, slice2) = splitST spltidx snap
  lv <- mySpawn $ lef slice1
  (rx, _) <- rig slice2
  (lx, _) <- PC.get lv
  return ((lx, rx), snap) -- FIXME: Should we ignore modified states?

-- | Spawn which does not assume idempotency of forked computations:
mySpawn :: (HasPut e, PC.ParIVar p) => p e s a -> p e s (PC.IVar p s a)
mySpawn f =
  do l <- PC.new
     PC.fork (do x <- f; PC.putNI_ l x)
     return l

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
  {-# INLINE putNI_ #-}
  putNI_ iv v = ParST $ \st -> (,st) <$> PC.putNI_ iv v


--------------------------------------------------------------------------------
-- | Generic way to build an in-place map operation for a collection state.
--
--   This function reserves the right to sequentialize some iterations.
mkParMapM :: forall elt s1 stt p e s .
             (STSplittable stt, ParThreadSafe p,
              PC.ParFuture p, HasGet e, HasPut e,
              PC.ParIVar p) =>
              (forall s2 . Int ->        ParST (stt s2) p e s elt) -- ^ Reader
           -> (forall s2 . Int -> elt -> ParST (stt s2) p e s ())  -- ^ Writer
           -> (forall s2 .               ParST (stt s2) p e s Int) -- ^ Length
           -> (Int -> SplitIdx stt)                                -- ^ Split elements
           -> (elt -> p e s elt)                                   -- ^ Fn to map over elmts.
           -> ParST (stt s1) p e s ()
{-# INLINE mkParMapM #-}
mkParMapM reader writer getsize mksplit fn = do
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
                iters1 = iters2 + extra
            R.void $ forkSTSplit (mksplit iters1) (loopmpm iters1) (loopmpm iters2)

  loopmpm len
