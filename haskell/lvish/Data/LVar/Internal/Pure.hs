{-# LANGUAGE Unsafe #-}

{-# LANGUAGE DataKinds, BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE InstanceSigs, MagicHash #-}

{-|

This is /not/ a datatype for the end-user.

Rather, this module is for building /new/ LVar types in a comparatively easy way: by
putting a pure value in a mutable container, and defining a @put@ operation as a pure
function.

The data structure implementor who uses this module must guarantee
that their @put@ operation computes a /least upper bound/, ensuring
that the set of states that their LVar type can take on form a
join-semilattice (<http://en.wikipedia.org/wiki/Semilattice>).

-}

module Data.LVar.Internal.Pure
       ( PureLVar(..),
         newPureLVar, putPureLVar, waitPureLVar, freezePureLVar
       ) where

import Control.LVish
import Control.LVish.DeepFrz.Internal
import Control.LVish.Internal
import Data.IORef
import qualified Control.LVish.SchedIdempotent as LI 
import Algebra.Lattice
import           GHC.Prim (unsafeCoerce#)

--------------------------------------------------------------------------------

-- | An LVar which consists merely of an immutable, pure value inside a mutable box.
newtype PureLVar s t = PureLVar (LVar s (IORef t) t)

-- data PureLVar s t = BoundedJoinSemiLattice t => PureLVar (LVar s (IORef t) t)

{-# INLINE newPureLVar #-}
{-# INLINE putPureLVar #-}
{-# INLINE waitPureLVar #-}
{-# INLINE freezePureLVar #-}

-- | A new pure LVar populated with the provided initial state.
newPureLVar :: BoundedJoinSemiLattice t =>
               t -> Par d s (PureLVar s t)
newPureLVar st = WrapPar$ fmap (PureLVar . WrapLVar) $
                 LI.newLV $ newIORef st

-- | Wait until the pure LVar has crossed a threshold and then unblock.  (In the
-- semantics, this is a singleton query set.)
waitPureLVar :: (JoinSemiLattice t, Eq t) =>
                PureLVar s t -> t -> Par d s ()
waitPureLVar (PureLVar (WrapLVar iv)) thrsh =
   WrapPar$ LI.getLV iv globalThresh deltaThresh
  where globalThresh ref _ = do
          x <- readIORef ref
          logDbgLn_ 5 "  [Pure] checking global thresh..."
          deltaThresh x
        deltaThresh x | thrsh `joinLeq` x = do logDbgLn_ 5 "  [Pure] Delta thresh met!"
                                               return $ Just ()
                      | otherwise         = do logDbgLn_ 5 "  [Pure] Check Delta thresh.. Not yet."
                                               return Nothing 

-- | Put a new value which will be joined with the old.
putPureLVar :: JoinSemiLattice t =>
               PureLVar s t -> t -> Par d s ()
putPureLVar (PureLVar (WrapLVar iv)) !new =
    WrapPar $ LI.putLV iv putter
  where
    -- Careful, this must be idempotent...
    putter !ref = do
      -- In some cases direct CAS would be better than atomicModifyIORef here.
      logDbgLn_ 5 "  [Pure] Putting to pure LVar.."
      atomicModifyIORef' ref $ \ oldval -> (join oldval new, ())
      -- We still publish the change for delta-thresh's to respond to:
      return $! Just $! new

-- | Freeze the pure LVar, returning its exact value.
--   Subsequent @put@s will raise an error.
freezePureLVar :: PureLVar s t -> Par QuasiDet s t
freezePureLVar (PureLVar (WrapLVar lv)) = WrapPar$ 
  do LI.freezeLV lv
     LI.getLV lv globalThresh deltaThresh
  where
    globalThresh ref True = fmap Just $ readIORef ref
    globalThresh _  False = return Nothing
    deltaThresh  _        = return Nothing

------------------------------------------------------------

-- | Physical identity, just as with `IORef`s.
instance Eq (PureLVar s v) where
  PureLVar lv1 == PureLVar lv2 = state lv1 == state lv2 

-- `PureLVar` values can be returned as the result of a
--  `runParThenFreeze`.  Hence they need a `DeepFrz` instance.
--  @DeepFrz@ is just a type-coercion.  No bits flipped at runtime.
instance DeepFrz a => DeepFrz (PureLVar s a) where
  -- We can't be sure that someone won't put an LVar value inside a
  -- PureLVar!  Therefore we have to apply FrzType recursively.
  type FrzType (PureLVar s a) = PureLVar Frzn (FrzType a)
  frz = unsafeCoerce#

