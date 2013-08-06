{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-} -- DeepFreezable

-- | An optional type class that provides a way to freeze nested
--   monotonic data structures in one step.
-- 
--   The basic tradeoff is that the user saves work at the term level,
--   but must constraining everything at the type level (with
--   annotations), and dealing with the inevitable type errors
--   involving ambiguities, existential types, and GADTs.

module Control.LVish.DeepFreeze
    (DeepFreeze(..),
     DeepFreezable(..), DeepFreezable2(..), DeepFreezable3(..), 
     runDeepFreezable, runDeepFreezable2, runDeepFreezable3 )
    where

import           Control.LVish
import           Control.LVish.Internal
import qualified Control.LVish.SchedIdempotent as L
import           System.IO.Unsafe (unsafePerformIO)

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


-- | Ditto for THREE freezable values.
data DeepFreezable3 f a1 b1   g a2 b2   h a3 b3 =
  forall s . (DeepFreeze (f s a1) b1, LVarData1 f,
              DeepFreeze (g s a2) b2, LVarData1 g,
              DeepFreeze (h s a3) b3, LVarData1 h) =>
  DeepFreezable3 !(Par Det s (f s a1, g s a2, h s a3))


-- | This allows Deterministic Par computations to return LVars (which normally
-- cannot escape), and it implicitly does a deepFreeze on them on their way out.
runDeepFreezable :: DeepFreezable f a b -> b
runDeepFreezable (DeepFreezable p) = unsafeRunThenFreeze p

-- | Ditto for a parallel computation with TWO freezable result values.
runDeepFreezable2 :: DeepFreezable2 f a1 b1  g a2 b2 -> (b1,b2)
runDeepFreezable2 (DeepFreezable2 p) =
  unsafePerformIO $ runParThenFreezeIO2 p

-- | Ditto for a parallel computation with THREE freezable result values.
runDeepFreezable3 :: DeepFreezable3 f a1 b1  g a2 b2   h a3 b3 -> (b1,b2,b3)
runDeepFreezable3 (DeepFreezable3 p@(WrapPar pi)) = unsafePerformIO$ do
  (res1,res2,res3) <- L.runParIO pi
  v1 <- runParIO (unsafeConvert $ deepFreeze res1) -- Inefficient! TODO: run without worker threads.
  v2 <- runParIO (unsafeConvert $ deepFreeze res2) -- Inefficient! TODO: run without worker threads.
  v3 <- runParIO (unsafeConvert $ deepFreeze res3) -- Inefficient! TODO: run without worker threads.
  return (v1,v2,v3)

----------------------------------------------------------------
-- HIDING these (not exporting).  You can use quiesceAll and freeze yourself:
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


