{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DataKinds #-}  -- For Determinism
{-# LANGUAGE MagicHash #-}

module Control.LVish.Internal
  (
    Par(..), LVar(..), LVarData1(..),
    unWrapPar, unsafeRunPar, unsafeCoerceLVar,
    Determinism(..), 
    unsafeConvert, state,
    liftIO,
    for_
  )
  where

import           Control.Monad.IO.Class
import           Control.LVish.MonadToss
import           Control.Applicative
import qualified Control.LVish.SchedIdempotent as L
import           Control.LVish.DeepFrz.Internal (Frzn, Trvrsbl)
import qualified Data.Foldable    as F
import           GHC.Prim (unsafeCoerce#)

{-# INLINE state  #-}
{-# INLINE unsafeConvert #-}
{-# INLINE unWrapPar #-}
--------------------------------------------------------------------------------

-- | This datatype is promoted to type-level and used to indicate whether a `Par`
-- computation is guaranteed-deterministic, or only quasi-deterministic (i.e. might throw NonDeterminismExn).
data Determinism = Det | QuasiDet
  deriving Show

-- Use DataKinds promotion to constrain the phantom type argument to be what we want.
newtype Par :: Determinism -> * -> * -> * where
  WrapPar :: L.Par a -> Par d s a
  deriving (Monad, Functor, Applicative)
-- type Foo = Par Det -- This is fine.
-- type Bar = Par Int -- Nice type error for this.

newtype LVar s all delt = WrapLVar { unWrapLVar :: L.LVar all delt }

unWrapPar :: Par d s a -> L.Par a
unWrapPar (WrapPar p) = p 

unsafeRunPar :: Par d s a -> a
unsafeRunPar p = L.runPar (unWrapPar p)

state :: LVar s a d -> a
state = L.state . unWrapLVar

-- | Ignore the extra type annotations regarding both determinism and session-sealing.
unsafeConvert :: Par d1 s1 a -> Par d2 s2 a
unsafeConvert (WrapPar p) = (WrapPar p)

instance MonadIO (Par d s) where
  liftIO = WrapPar . L.liftIO   

instance MonadToss (Par d s) where
  toss = WrapPar L.toss

-- My own forM for numeric ranges (not requiring deforestation optimizations).
-- Inclusive start, exclusive end.
{-# INLINE for_ #-}
for_ :: Monad m => (Int, Int) -> (Int -> m ()) -> m ()
for_ (start, end) _fn | start > end = error "for_: start is greater than end"
for_ (start, end) fn = loop start
  where
  loop !i | i == end  = return ()
          | otherwise = do fn i; loop (i+1)
  


------------------------------------------------------------------------------
-- Interface for generic LVar handling
------------------------------------------------------------------------------

-- | A class representing monotonic data types that take one type
-- parameter as well as an `s` parameter for session safety.
-- 
class (F.Foldable (f Trvrsbl)) => LVarData1 (f :: * -> * -> *)
     --   TODO: if there is a Par to generalize LVar Par monads, then
     --   it needs to be a superclass of this.
     where
  -- | The a frozen LVar provides a complete picture of the contents:
  -- e.g. a whole set instead of one element, or the full/empty
  -- information for an IVar, instead of just the payload.
  
  -- type LVCtxt (f :: * -> * -> *) (s :: *) (a :: *) :: Constraint
  --  I was not able to get abstracting over the constraints to work.

  freeze :: -- LVCtxt f s a =>
            f s a -> Par QuasiDet s (f Frzn a)
            
  newBottom :: Ord a => Par d s (f s a)

  addHandler :: Maybe L.HandlerPool -> f s elt -> (elt -> Par d s ()) -> Par d s ()

{- 
-- | Just like LVarData1 but for type constructors of kind `*`.
class LVarData0 (t :: *) where
  -- | This associated type models a picture of the "complete" contents of the data:
  -- e.g. a whole set instead of one element, or the full/empty information for an
  -- IVar, instead of just the payload.
  type Snapshot0 t
  freeze0 :: t -> Par QuasiDet s (Snapshot0 t)
  newBottom0 :: Par d s t
-}

-- | A safer version of `unsafeCoerce#` for LVars.
unsafeCoerceLVar :: LVarData1 f => f s1 a -> f s2 a
unsafeCoerceLVar = unsafeCoerce#
