{-# LANGUAGE DataKinds, BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE InstanceSigs, MagicHash #-}

{-|

This is NOT a datatype for the end-user.

Rather, this module is for building /new/ LVar types in a comparatively easy way: by
putting a pure value in a mutable container, and defining a LUB operation as a pure
function.

The proof-obligation for the library-writer who uses this module is that they must
guarantee that their LUB is a /true least-upper-bound/, obeying the appropriate laws
for a join-semilattice:

 * <http://en.wikipedia.org/wiki/Semilattice>

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


-- | Physical identity, just as with IORefs.
instance Eq (PureLVar s v) where
  PureLVar lv1 == PureLVar lv2 = state lv1 == state lv2 

-- | A `PureLVar` can be treated as a generic container LVar which happens to
-- contain exactly one value!
  
-- instance LVarData1 PureLVar where
--   freeze orig@(PureLVar (WrapLVar lv)) = WrapPar$ do freezeLV lv; return (unsafeCoerceLVar orig)
--   sortFreeze is = AFoldable <$> freezeSet is
--   addHandler = forEachHP

-- -- | The `PureLVar`s in this module also have the special property that they support an
-- -- `O(1)` freeze operation which immediately yields a `Foldable` container
-- -- (`snapFreeze`).
-- instance OrderedLVarData1 PureLVar where
--   snapFreeze is = unsafeCoerceLVar <$> freeze is

-- -- | As with all LVars, after freezing, map elements can be consumed. In the case of
-- -- this `PureLVar` implementation, it need only be `Frzn`, not `Trvrsbl`.
-- instance F.Foldable (PureLVar Frzn) where
--   foldr fn zer (PureLVar lv) =
--     -- It's not changing at this point, no problem if duped:
--     let set = unsafeDupablePerformIO (readIORef (state lv)) in
--     F.foldr fn zer set 

-- -- | Of course, the stronger `Trvrsbl` state is still fine for folding.
-- instance F.Foldable (PureLVar Trvrsbl) where
--   foldr fn zer mp = F.foldr fn zer (castFrzn mp)

-- | `PureLVar` values can be returned as the result of a `runParThenFreeze`.
--   Hence they need a `DeepFrz` instace.
--   @DeepFrz@ is just a type-coercion.  No bits flipped at runtime.
instance DeepFrz a => DeepFrz (PureLVar s a) where
  type FrzType (PureLVar s a) = PureLVar Frzn a -- No recursion, the contents are pure.
  frz = unsafeCoerce#

