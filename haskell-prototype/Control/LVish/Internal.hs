{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DataKinds #-}  -- For Determinism
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE RankNTypes #-}

module Control.LVish.Internal
  (
    Par(..), LVar(..),
    LVarData1(..), OrderedLVarData1(..), AFoldable(..),
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
import           Data.List (sort)
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
-- LVars that fall into this class are typically collection types.
class (F.Foldable (f Trvrsbl)) => LVarData1 (f :: * -> * -> *)
     --   TODO: if there is a Par class to generalize LVar Par monads, then
     --   it needs to be a superclass of this.
     where
  -- | The a frozen LVar provides a complete picture of the contents:
  -- e.g. a whole set instead of one element, or the full/empty
  -- information for an IVar, instead of just the payload.
  
  -- type LVCtxt (f :: * -> * -> *) (s :: *) (a :: *) :: Constraint
  --  I was not able to get abstracting over the constraints to work.

  newBottom :: Ord a => Par d s (f s a)

  -- | Add a handler function which is called whenever an element is
  -- added to the LVar.
  addHandler :: Maybe L.HandlerPool -> f s elt -> (elt -> Par d s ()) -> Par d s ()

  -- | An /O(1)/ operation that atomically switches the LVar into a
  -- frozen state.  Any threads waiting on the freeze are woken.
  freeze :: -- LVCtxt f s a =>
            f s a -> Par QuasiDet s (f Frzn a)

  -- | Perform a freeze followed by a /sort/ operation which guarantees
  -- that the elements produced will be produced in a deterministic order.
  -- The result is fully accessible (Foldable).
  sortFreeze :: forall a s . Ord a => f s a -> Par QuasiDet s (AFoldable a)
  -- sortFreeze :: forall a s . Ord a => f s a -> Par QuasiDet s [a]
  sortFreeze lv = do 
    lv2 <- freeze lv
    let lv3 :: f Trvrsbl a
        lv3 = unsafeCoerceLVar lv2
        ls  = F.foldr (:) [] lv3
        ls' = sort ls
    -- Without a traversible instance we cannot reconstruct an ordered
    -- version of the LVar contents with its original type:
    return (AFoldable ls')

-- | Carries a Foldable type, but you don't know which one.
data AFoldable a = forall f2 . F.Foldable f2 => AFoldable (f2 a)

-- | Some LVar datatypes are stored in an internally ordered way so
-- that it is then possible to take frozen snapshots and consume them
-- inexpensively in a deterministic order.
class LVarData1 f => OrderedLVarData1 (f :: * -> * -> *) where
  -- | Don't just freeze the LVar, but make the full contents
  -- completely available and Foldable.  Guaranteed /O(1)/.
  snapFreeze :: f s a -> Par QuasiDet s (f Trvrsbl a)

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
