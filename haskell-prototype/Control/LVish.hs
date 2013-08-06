{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}  -- For 'Determinism'
-- {-# LANGUAGE ConstraintKinds, KindSignatures #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

{-# LANGUAGE GADTs #-} -- DeepFreezable

-- | A module that reexports the default LVish scheduler, adding some type-level
-- wrappers to ensure propert treatment of determinism.

module Control.LVish
  (
    -- * Basic types and accessors:
    LVar(), state, L.HandlerPool(), Par(), 
    Determinism(..), liftQ,
    
    -- * Safe, deterministic operations:
    yield, newPool, fork, forkHP,
    runPar, runParIO, runParIO_, runParLogged,
--    unsafeRunThenFreeze, runParThenFreezeIO, runParThenFreezeIO2,
    withNewPool, withNewPool_,
    
    -- * Quasi-deterministic operations:
    quiesce, quiesceAll,
    
    -- * Interfaces for generic operations
    LVarData1(..), DeepFreeze(..),
    DeepFreezable(..),runDeepFreezable, 

    DeepFreezable2(..), FreezableTuple(..), -- TEMPORARY!
    
    -- * Generally useful combinators
    parForL, parForSimple, parForTree, parForTiled, for_,
    
    -- * Debug facilities
    logStrLn
  ) where

import           Control.Applicative
import           Control.LVish.Internal
import qualified Control.LVish.SchedIdempotent as L
import           System.IO.Unsafe (unsafePerformIO)

-- import GHC.Exts (Constraint)

--------------------------------------------------------------------------------
-- Inline *everything*, because these are just wrappers:
{-# INLINE liftQ #-}
{-# INLINE yield #-}
{-# INLINE newPool #-}
{-# INLINE runParIO #-}
{-# INLINE runPar #-}
-- {-# INLINE runParThenFreeze #-}
{-# INLINE fork #-}
{-# INLINE quiesce #-}
--------------------------------------------------------------------------------

-- | It is always safe to lift a deterministic computation to a quasi-determinism one.
liftQ :: Par Det s a -> Par QuasiDet s a
liftQ (WrapPar p) = (WrapPar p)

yield :: Par d s ()
yield = WrapPar L.yield

quiesce :: L.HandlerPool -> Par d s ()
quiesce = WrapPar . L.quiesce

-- | A global barrier.
quiesceAll :: Par d s ()
quiesceAll = WrapPar L.quiesceAll

fork :: Par d s () -> Par d s ()
fork (WrapPar f) = WrapPar$ L.fork f

forkHP :: Maybe L.HandlerPool -> Par d s () -> Par d s ()
forkHP mh (WrapPar f) = WrapPar$ L.forkHP mh f

newPool :: Par d s L.HandlerPool
newPool = WrapPar L.newPool

withNewPool :: (L.HandlerPool -> Par d s a) -> Par d s (a, L.HandlerPool)
withNewPool f = WrapPar $ L.withNewPool $ unWrapPar . f

withNewPool_ :: (L.HandlerPool -> Par d s ()) -> Par d s L.HandlerPool
withNewPool_ f = WrapPar $ L.withNewPool_ $ unWrapPar . f

-- | If the input computation is quasi-deterministic, this may throw
-- 'NonDeterminismExn' on the thread that calls it.
runParIO :: (forall s . Par d s a) -> IO a
runParIO (WrapPar p) = L.runParIO p 

-- | Useful ONLY for timing.
runParIO_ :: (Par d s a) -> IO ()
runParIO_ (WrapPar p) = L.runParIO p >> return ()

-- | Useful for debugging.  Returns debugging logs, in realtime order, in addition to
-- the final result.
runParLogged :: (forall s . Par d s a) -> IO ([String],a)
runParLogged (WrapPar p) = L.runParLogged p 

runPar :: (forall s . Par Det s a) -> a
runPar (WrapPar p) = L.runPar p 

-- | This version is internal/unsafe because it does not seal up the
-- 's' parameter.
unsafeRunThenFreeze :: (DeepFreeze (f s a) b, LVarData1 f) =>
                    Par Det s (f s a) -> b
unsafeRunThenFreeze p = unsafePerformIO$ runParThenFreezeIO p

-- | This datatype exists to allow the user to package computations with
-- freezable results so that they may be run, discharging the 's' parameter.
data DeepFreezable f a b = forall s . (DeepFreeze (f s a) b, LVarData1 f) =>
                           DeepFreezable !(Par Det s (f s a))

-- | The same trick, but allows TWO freezable values to be returned
-- from the computation.
data DeepFreezable2 f a1 b1  g a2 b2 =
  forall s . (DeepFreeze (f s a1) b1, LVarData1 f,
              DeepFreeze (g s a2) b2, LVarData1 g) =>
  DeepFreezable2 !(Par Det s (f s a1, g s a2))

-- Can we avoid DeepFreezable2 and reuse DeepFreezable?
data FreezableTuple s a where
    -- forall f g . (LVarData1 f, LVarData1 g) =>
    -- -- We really need two different 'a's... but that means having a
    -- -- type level Fst and Snd.
    -- FreezableTuple2 (f s (Fst a)) (g s (Snd a))

    FreezableTuple2 :: 
     forall f g s a b . (LVarData1 f, LVarData1 g) =>
     (f s a) -> (g s b) -> FreezableTuple s (f s a, g s b)

    FreezableTuple3 :: 
     forall s  f g h   a b c . (LVarData1 f, LVarData1 g) =>
     (f s a) -> (g s b) -> (h s c) -> FreezableTuple s (f s a, g s b, h s c)

instance LVarData1 FreezableTuple where
  newtype Snapshot FreezableTuple a = TupleSnap a
  freeze ft = case ft of
                FreezableTuple2 fa gb    -> return (TupleSnap (fa,gb))
                FreezableTuple3 fa gb hc -> return (TupleSnap (fa,gb,hc))
  traverseSnap fn (TupleSnap a) = TupleSnap <$> fn a


instance (DeepFreeze (f s a1) b1, LVarData1 f,
          DeepFreeze (g s a2) b2, LVarData1 g) =>
         DeepFreeze (FreezableTuple s (f s a1, g s a2)) (b1,b2) where
  type Session (FreezableTuple s (f s a1, g s a2)) = s

  deepFreeze :: FreezableTuple s (f s a1, g s a2) -> 
                Par QuasiDet (Session (FreezableTuple s (f s a1, g s a2))) (b1, b2)
#if 0
  deepFreeze ft@(FreezableTuple2 (fa) (ga)) =
    case (ft :: FreezableTuple s (f s a1, g s a2)) of
      FreezableTuple2 (fa :: f s a1) (ga) -> undefined
#elif 1
  deepFreeze (ft :: FreezableTuple s (f s a1, g s a2)) =
    case ft of
      --
      FreezableTuple2 fa ga ->
--      FreezableTuple2 (fa :: f s a) (ga) ->
    -- Could not deduce (f1 ~ f)
    -- from the context (DeepFreeze (f s a1) b1,
    --                   LVarData1 f,
    --                   DeepFreeze (g s a2) b2,
    --                   LVarData1 g)
   -- or from ((f s a1, g s a2) ~ (f1 s a, g1 s b),
   --           LVarData1 f1,
   --           LVarData1 g1)
    
        (error "Finishme")
#else
  deepFreeze (FreezableTuple2 (fa :: f s a1) (ga :: g s a2)) =
    unsafeConvert $ do 
      x1 <- deepFreeze fa
      x2 <- deepFreeze ga
      return (x1,x2)
#endif

-- type family TupleType :: * -> *
-- newtype instance TupleType Int = Float

-- type family Fst :: * -> *
type family Fst ty
type instance Fst (a,b) = a
type instance Fst (a,b,c) = a
type instance Fst (a,b,c,d) = a
type instance Fst (a,b,c,d,e) = a
type instance Fst (a,b,c,d,e,f) = a
type instance Fst (a,b,c,d,e,f,g) = a

type family Snd ty
type instance Snd (a,b) = b
type instance Snd (a,b,c) = b
type instance Snd (a,b,c,d) = b
type instance Snd (a,b,c,d,e) = b
type instance Snd (a,b,c,d,e,f) = b
type instance Snd (a,b,c,d,e,f,g) = b

-- Example:
x :: Fst (Int,Float)
x = 3

y :: Snd (Int,Float,Char)
y = 3.3


-- | This allows Deterministic Par computations to return LVars (which normally
-- cannot escape), and it implicitly does a deepFreeze on them on their way out.
runDeepFreezable :: DeepFreezable f a b -> b


runDeepFreezable (DeepFreezable p) = unsafeRunThenFreeze p

----------------------------------------------------------------
-- DISABLING these.  You can use quiesceAll and freeze yourself:
----------------------------------------------------------------
-- | This version works for quasi-deterministic computations as well.  Such
--   computations may also do freezes internally, but this function has an advantage
--   vs. doing your own freeze at the end of your computation.  Namely, when you use
--   `runParThenFreezeIO`, there is an implicit barrier before the final freeze.
runParThenFreezeIO :: (DeepFreeze (f s a) b, LVarData1 f) =>
                       Par d s (f s a) -> IO b
runParThenFreezeIO par = do 
  res <- L.runParIO (unWrapPar par)
  runParIO (unsafeConvert $ deepFreeze res) -- Inefficient! TODO: run without worker threads.

-- | If DeepFreeze were flexible enough, this should not be necessary.
runParThenFreezeIO2 :: (DeepFreeze (f s a) b, LVarData1 f,
                        DeepFreeze (g s c) d, LVarData1 g) =>
                    Par det s (f s a, g s c) -> IO (b,d)
runParThenFreezeIO2 par@(WrapPar pi) = do
  (res1,res2) <- L.runParIO pi
  v1 <- runParIO (unsafeConvert $ deepFreeze res1) -- Inefficient! TODO: run without worker threads.
  v2 <- runParIO (unsafeConvert $ deepFreeze res2) -- Inefficient! TODO: run without worker threads.
  return (v1,v2)
----------------------------------------------------------------


logStrLn :: String -> Par d s ()
#ifdef DEBUG_LVAR
logStrLn = WrapPar . L.logStrLn
#else 
logStrLn _  = return ()
{-# INLINE logStrLn #-}
#endif




------------------------------------------------------------------------------
-- Interface for generic LVar handling
------------------------------------------------------------------------------

-- class Traversable f => LVarData1 (f :: * -> *) where

-- | A class representing monotonic data types that take one type
-- parameter as well as an `s` parameter for session safety.
-- 
--   TODO: if there is a Par class, it needs to be a superclass of this.
class LVarData1 (f :: * -> * -> *) where
  -- | This associated type models a picture of the "complete" contents of the data:
  -- e.g. a whole set instead of one element, or the full/empty information for an
  -- IVar, instead of just the payload.
  -- type Snapshot f a :: *
  data Snapshot f :: * -> *
  -- type LVCtxt (f :: * -> * -> *) (s :: *) (a :: *) :: Constraint
  --  I was not able to get abstracting over the constraints to work.

  freeze :: -- LVCtxt f s a =>
            f s a -> Par QuasiDet s (Snapshot f a)
  newBottom :: Ord a => Par d s (f s a)

  -- QUESTION: Is there any way to assert that the snapshot is still Traversable?
  -- I don't know of a good way, so instead we expose this:

  -- | 
  traverseSnap :: (a -> Par d s b) -> Snapshot f a -> Par d s (Snapshot f b)
  -- TODO: use constraint kinds to constrain 'b'.

  -- foldSnap :: (a -> b -> Par d s b) -> b -> f s a -> Par d s b

  -- What else?
  -- Merge op?


-- This gets messy if we try to handle several Kinds:

-- | Just like LVarData1 but for type constructors of kind `*`.
class LVarData0 (t :: *) where
  -- | This associated type models a picture of the "complete" contents of the data:
  -- e.g. a whole set instead of one element, or the full/empty information for an
  -- IVar, instead of just the payload.
  type Snapshot0 t
  freeze0 :: t -> Par QuasiDet s (Snapshot0 t)
  newBottom0 :: Par d s t

--------------------------------------------------------------------------------
-- Freezing nested structures in one go
--------------------------------------------------------------------------------

-- | This establishes an unrestricted *relation* between input and output types.  Thus
-- it is powerful, but can be painful to use.  The input and output types of
-- deepFreeze must be fully constrained at every call site.  This allows the user to
-- potentially freeze a nested structure in various ways of their choosing.
class DeepFreeze (from :: *) (to :: *) where
  type Session from 
  deepFreeze :: from -> Par QuasiDet (Session from) to


instance forall f g a s .
         (LVarData1 f, LVarData1 g) =>
         DeepFreeze (f s (g s a)) (Snapshot f (Snapshot g a)) where
  type Session (f s (g s a)) = s
  deepFreeze lvd = unsafeConvert par
    where
      -- RRN: Type signatures here are not in the scope of the above forall... ergh.
      -- par :: Par QuasiDet s (Snapshot f (Snapshot g a))
      par = do
        x <- freeze lvd            -- :: QPar s (Snapshot f (g a))
        y <- traverseSnap freeze x -- :: QPar s (Snapshot f (Snapshot g a))
        return y

type QPar = Par QuasiDet 

-- Inherit everything that regular freeze can do:
instance LVarData1 f => DeepFreeze (f s a) (Snapshot f a) where
  type Session (f s a) = s
  deepFreeze = unsafeConvert . freeze

--------------------------------------------------------------------------------
-- Extras
--------------------------------------------------------------------------------

{-# INLINE parForL #-}
-- | Left-biased parallel for loop.  As worker threads beyond the first are added,
-- this hews closer to the sequential iteration order than an unbiased parallel loop.
--
-- Takes a range as inclusive-start, exclusive-end.
parForL :: (Int,Int) -> (Int -> Par d s ()) -> Par d s ()
parForL (start,end) body | start > end = error$"parForL: start is greater than end: "++show (start,end)
parForL (start,end) body = do
  -- logStrLn$ " initial iters: "++show (end-start)
  loop 0 (end - start) 1
 where
   loop offset remain chunk
     | remain <= 0     = return () 
     | remain <= chunk = parForSimple (offset, offset+remain) body
     | otherwise       = do
         let nxtstrt = offset+chunk
         -- logStrLn$ "loop:  .. "++show (offset, remain, chunk)
         fork$ parForSimple (offset, nxtstrt) body
         loop nxtstrt (remain-chunk) (2*chunk)

   -- forSlice = parForSimple
   -- forSlice = parForTree
   forSlice = parForL

{-# INLINE parForSimple #-}
-- | The least-sophisticated form of parallel loop.  Fork iterations one at a time.
parForSimple :: (Int,Int) -> (Int -> Par d s ()) -> Par d s ()
parForSimple range fn = do
  for_ range $ \i -> fork (fn i) 

-- | Divide the iteration space recursively, but ultimately run every iteration in
-- parallel.  That is, the loop body is permitted to block on other iterations.
parForTree :: (Int,Int) -> (Int -> Par d s ()) -> Par d s ()
parForTree (start,end) body | start > end = error$"parForTree: start is greater than end: "++show (start,end)
parForTree (start,end) body = do
  loop 0 (end - start)
 where
   loop offset remain 
     | remain == 1     = body offset
     | otherwise       = do
         let (half,rem) = remain `quotRem` 2
         fork$ loop offset half
         loop (offset+half) (half+rem)


-- | Split the work into a number of tiles, and fork it in a tree topology.
parForTiled :: Int -> (Int,Int) -> (Int -> Par d s ()) -> Par d s ()
parForTiled tiles (start,end) body = do 
  loop 0 (end - start) tiles
 where
   loop offset remain tiles
     | remain == 1     = body offset
     | tiles  == 1     = for_ (offset,offset+remain) body
     | otherwise       = do
         let (half,rem)   = remain `quotRem` 2
             (halfT,remT) = tiles `quotRem` 2
         fork$ loop offset half halfT
         loop (offset+half) (half+rem) (halfT+remT)
