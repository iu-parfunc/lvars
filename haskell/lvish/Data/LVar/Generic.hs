{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}  -- For Determinism

{-# LANGUAGE FlexibleInstances #-}

-- | A generic interface providing operations that work on ALL LVars.

module Data.LVar.Generic
       (
         -- * The classes containing the generic interfaces
         LVarData1(..), OrderedLVarData1(..),
         
         -- * Supporting types and utilities
         AFoldable(..),
         castFrzn, forFrzn
       )
       where

import           Control.LVish
import           Control.LVish.DeepFrz.Internal (Frzn, Trvrsbl)
import qualified Data.Foldable    as F
import           Data.List (sort, intersperse)
import           GHC.Prim (unsafeCoerce#)
import           System.IO.Unsafe (unsafeDupablePerformIO)
import           Data.LVar.Generic.Internal

--------------------------------------------------------------------------------

-- |/Some LVar datatypes are stored in an /internally/ ordered way so
-- that it is then possible to take /O(1)/ frozen snapshots and consume them
-- inexpensively in a deterministic order.
--
-- LVars with this additional property provide this class as well as `LVarData1`.
class LVarData1 f => OrderedLVarData1 (f :: * -> * -> *) where
  -- | Don't just freeze the LVar, but make the full contents
  -- completely available and Foldable.  Guaranteed /O(1)/.
  snapFreeze :: f s a -> Par QuasiDet s (f Trvrsbl a)

instance (Ord a, LVarData1 f, Show a) => Show (f Trvrsbl a) where
  show lv =
        "{" ++
        (concat $ intersperse ", " $ 
         F.foldr (\ elm ls -> show elm : ls) [] lv) ++ "}"

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


------------------------------------------------------------------------------
-- Dealing with frozen LVars.
------------------------------------------------------------------------------

-- | `Trvrsbl` is a stronger property than `Frzn` so it is always ok to \"upcast\" to
-- the weaker version.
castFrzn :: LVarData1 f => f Trvrsbl a -> f Frzn a
castFrzn x = unsafeCoerceLVar x

-- | LVish Par actions must commute, therefore one safe way to consume a frozen (but
-- unordered) LVar, /even in another runPar session/, is to run a par computation for
-- each element.
forFrzn :: LVarData1 f => f Frzn a -> (a -> Par d s ()) -> Par d s ()
forFrzn fzn fn =
  F.foldrM (\ a () -> fn a) () $ 
    unsafeDupablePerformIO $ -- ASSUME idempotence.
    unsafeTraversable fzn


-- | For any LVar, we have a generic way to freeze it in a `runParThenFreeze`.
-- instance (DeepFrz a, LVarData1 f) => DeepFrz (f s a) where
--   type FrzType (f s a) = f Frzn a 
--   frz = unsafeCoerceLVar

-- ^^^

-- Note that this doesn't work because it CONFLICTS with the other DeepFrz instances.
-- There's no way that we can prove to GHC that pure data will NEVER be an instance
-- of LVarData1, and therefore will never actually cause a conflict with e above
-- instance.
