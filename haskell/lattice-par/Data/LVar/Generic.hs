{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}  -- For Determinism
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A generic interface that abstracting certain operations that work on all LVars.

module Data.LVar.Generic
       (
         LVarData1(..), OrderedLVarData1(..), AFoldable(..),
         unsafeCoerceLVar,
         mkTraversable, forFrzn
       )
       where

import           Control.LVish
import           Control.Monad.IO.Class
import           Control.LVish.MonadToss
import           Control.Applicative
import qualified Control.LVish.SchedIdempotent as L
import           Control.LVish.DeepFrz.Internal (Frzn, Trvrsbl)
import qualified Data.Foldable    as F
import           Data.List (sort)

import           GHC.Prim (unsafeCoerce#)
import           System.IO.Unsafe (unsafeDupablePerformIO)

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

------------------------------------------------------------------------------
-- Dealing with frozen LVars.
------------------------------------------------------------------------------

-- | Here we pay the piper with an IO action, gaining permission to
-- expose the non-deterministic internal structure of an LVar: namely,
-- the order in which elements occur.
mkTraversable :: LVarData1 f => f Frzn a -> IO (f Trvrsbl a)
mkTraversable x = return (unsafeCoerceLVar x) 

-- | LVish Par actions must commute, therefore one safe way to consume a frozen (but
-- unordered) LVar, *even in another runPar session*, is to run a par computation for
-- each element.
forFrzn :: LVarData1 f => f Frzn a -> (a -> Par d s ()) -> Par d s ()
forFrzn fzn fn =
  F.foldrM (\ a () -> fn a) () $ 
    unsafeDupablePerformIO $ -- ASSUME idempotence.
    mkTraversable fzn


-- | For any LVar, we have a generic way to freeze it in a `runParThenFreeze`.
-- instance (DeepFrz a, LVarData1 f) => DeepFrz (f s a) where
--   type FrzType (f s a) = f Frzn a 
--   frz = unsafeCoerceLVar

-- ^^^

-- Note that this doesn't work because it CONFLICTS with the other DeepFrz instances.
-- There's no way that we can prove to GHC that pure data will NEVER be ans instance
-- of LVarData1, and therefore will never actually cause a conflict with e above
-- instance.
