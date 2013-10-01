{-# LANGUAGE Unsafe #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}  -- For Determinism
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|

This module contains the unsafe bits that we cannot expose from 
  "Data.LVar.Generic".

-}

module Data.LVar.Generic.Internal
       (LVarData1(..), AFoldable(..),
        unsafeCoerceLVar, unsafeTraversable)
       where

import           Control.LVish
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
  -- type LVCtxt (f :: * -> * -> *) (s :: *) (a :: *) :: Constraint
  --  I was not able to get abstracting over the constraints to work.

  -- | Add a handler function which is called whenever an element is
  -- added to the LVar.
  addHandler :: Maybe HandlerPool -> f s elt -> (elt -> Par d s ()) -> Par d s ()

  -- | An /O(1)/ operation that atomically switches the LVar into a
  -- frozen state.  Any threads waiting on the freeze are woken.
  --
  -- The frozen LVar provides a complete picture of the contents:
  -- e.g. a whole set instead of one element, or the full/empty
  -- information for an IVar, instead of just the payload.
  --
  -- However, note that `Frzn` LVars cannot be folded, because they may have
  -- nondeterministic ordering after being frozen.  See `sortFreeze`.
  freeze :: -- LVCtxt f s a =>
            f s a -> Par QuasiDet s (f Frzn a)

  -- | Perform a freeze followed by a /sort/ operation which guarantees
  -- that the elements produced will be produced in a deterministic order.
  -- The result is fully accessible to the user (`Foldable`).
  sortFrzn :: Ord a => f Frzn a -> AFoldable a
  sortFrzn lv = 
    let lv3 :: f Trvrsbl a
        lv3 = unsafeCoerceLVar lv
        ls  = F.foldr (:) [] lv3
        ls' = sort ls
    -- Without a traversible instance we cannot reconstruct an ordered
    -- version of the LVar contents with its original type:
    in AFoldable ls'

-- | Carries a Foldable type, but you don't get to know which one.
--   The purpose of this type is that `sortFreeze` should not have
--   to impose a particular memory representation.
data AFoldable a = forall f2 . F.Foldable f2 => AFoldable (f2 a)

--------------------------------------------------------------------------------

{-# INLINE unsafeCoerceLVar #-}
-- | A safer version of `unsafeCoerce#` for LVars only.
--   Note that it needs to change the contents type, because freezing is recursive.
unsafeCoerceLVar :: LVarData1 f => f s1 a -> f s2 b
unsafeCoerceLVar = unsafeCoerce#

-- | Here we gain permission to expose the non-deterministic internal structure of an
-- LVar: namely, the order in which elements occur.  We pay the piper with an IO
-- action.
unsafeTraversable :: LVarData1 f => f Frzn a -> IO (f Trvrsbl a)
unsafeTraversable x = return (unsafeCoerceLVar x) 

