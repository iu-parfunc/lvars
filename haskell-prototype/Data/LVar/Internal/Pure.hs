{-# LANGUAGE DataKinds, BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

-- | This is not an end-user datatype.  This is for building new LVar types in a
-- comparatively easy way: by putting a pure value in a mutable container, and
-- defining a LUB operation as a pure function.

module Data.LVar.Internal.Pure
       ( PureLVar(..),
         newPureLVar, putPureLVar, waitPureLVar, freezePureLVar
       ) where

import Control.LVish
import Control.LVish.DeepFreeze
import Control.LVish.Internal
import Data.IORef
import qualified Control.LVish.SchedIdempotent as LI 
import Algebra.Lattice

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

-- | Wait until the Pure LVar has crossed a threshold and then unblock.  (In the
-- semantics, this is a singleton query set.)
waitPureLVar :: (JoinSemiLattice t, Eq t) =>
                PureLVar s t -> t -> Par d s ()
waitPureLVar (PureLVar (WrapLVar iv)) thrsh =
   WrapPar$ LI.getLV iv globalThresh deltaThresh
  where globalThresh ref _ = do x <- readIORef ref
                                deltaThresh x
        deltaThresh x | thrsh `joinLeq` x = return $ Just ()
                      | otherwise         = return Nothing 

-- | Put a new value which will be joined with the old.
putPureLVar :: JoinSemiLattice t =>
               PureLVar s t -> t -> Par d s ()
putPureLVar (PureLVar (WrapLVar iv)) !new =
    WrapPar $ LI.putLV iv putter
  where
    -- Careful, this must be idempotent...
    putter _ = return (Just new)

-- | Freeze the pure LVar, returning its exact value.
--   Subsequent puts will cause an error.
freezePureLVar :: PureLVar s t -> Par QuasiDet s t
freezePureLVar (PureLVar (WrapLVar lv)) = WrapPar$ 
  do LI.freezeLV lv
     LI.getLV lv globalThresh deltaThresh
  where
    globalThresh ref True = fmap Just $ readIORef ref
    globalThresh _  False = return Nothing
    deltaThresh  _        = return Nothing

------------------------------------------------------------

instance DeepFreeze (PureLVar s a) a where
  type Session (PureLVar s a) = s 
  deepFreeze = freezePureLVar

instance LVarData1 PureLVar where
  newtype Snapshot PureLVar a = PureSnap a
    deriving (Show,Ord,Read,Eq)
  
  freeze :: PureLVar s a -> Par QuasiDet s (Snapshot PureLVar a)
  freeze = unsafeConvert . fmap PureSnap . freezePureLVar

  -- newBottom :: -- BoundedJoinSemiLattice a =>
  --              Par d s (PureLVar s a)
  -- newBottom = error "Pure.hs - FINISHME"  -- newPureLVar bottom
  -- -- FIXME -- constraint kinds...
  
  traverseSnap f (PureSnap x) = fmap PureSnap (f x)

