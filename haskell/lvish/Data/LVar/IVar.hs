{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns, MultiParamTypeClasses, TypeFamilies, TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-} 

{-|

  IVars are the very simplest form of LVars.  They are either empty or full, and
  contain at most a single value.

  For further information on using IVars in Haskell, see the @monad-par@ and
  @meta-par@ packages and papers:

    * <http://hackage.haskell.org/package/monad-par>

    * <http://www.cs.indiana.edu/~rrnewton/papers/haskell2011_monad-par.pdf>

    * <http://hackage.haskell.org/package/meta-par>

    * <http://www.cs.indiana.edu/~rrnewton/papers/2012-ICFP_meta-par.pdf>

Unlike the @IVar@ type provided by @monad-par@, the 'IVar' type
provided in this module permits repeated `put`s of the same value, in
keeping with the lattice-based semantics of LVars in which a `put`
takes the least upper bound of the old and new values.

 -}

module Data.LVar.IVar
       (
         IVar(..),
         -- * Basic IVar operations, same as in monad-par
         new, get, put, put_,
         
         -- * Derived IVar operations, same as in monad-par
        spawn, spawn_, spawnP,

        -- * LVar-style operations
        freezeIVar, fromIVar, whenFull)
       where

import           Data.IORef
import           Control.DeepSeq
import           System.Mem.StableName (makeStableName, hashStableName)
import           System.IO.Unsafe      (unsafePerformIO, unsafeDupablePerformIO)
import qualified Data.Foldable    as F
import           Control.Exception (throw)
import qualified Control.LVish.Types as LV 
import qualified Control.LVish.Basics as LV 
import           Control.LVish.DeepFrz.Internal
import           Control.LVish.Internal as I
import           Control.LVish.SchedIdempotent (newLV, putLV, getLV, freezeLV)
import qualified Control.LVish.SchedIdempotent as LI 
import           Data.LVar.Generic
import           Data.LVar.Generic.Internal (unsafeCoerceLVar)
import           GHC.Prim (unsafeCoerce#)

#ifdef GENERIC_PAR
import qualified Control.Par.Class as PC
#endif

------------------------------------------------------------------------------
-- IVars implemented on top of (the idempotent implementation of) LVars
------------------------------------------------------------------------------
       
-- | An `IVar` is the simplest form of `LVar`.
newtype IVar s a = IVar (LVar s (IORef (Maybe a)) a)
-- the global data for an IVar a is a reference to Maybe a, while deltas are
-- simply values of type a (taking the IVar from Nothing to Just):

-- | Physical equality, just as with `IORef`s.
instance Eq (IVar s a) where
  (==) (IVar lv1) (IVar lv2) = state lv1 == state lv2

-- | An `IVar` can be treated as a generic container LVar which happens to
-- contain at most one value!  Note, however, that the polymorphic operations are
-- less useful than the monomorphic ones exposed by this module.
instance LVarData1 IVar where  
  freeze :: IVar s a -> Par QuasiDet s (IVar Frzn a)
  freeze orig@(IVar (WrapLVar lv)) = WrapPar $ do
    freezeLV lv
    return (unsafeCoerceLVar orig)
  addHandler = whenFull

instance LVarWBottom IVar where
  type LVContents IVar a = ()
  newBottom = new

-- Just a type-coercion.  No bits flipped at runtime.
instance DeepFrz a => DeepFrz (IVar s a) where
  type FrzType (IVar s a) = IVar Frzn (FrzType a)
  frz = unsafeCoerceLVar

-- As with all other `Trvrsbl` LVars, the elements are traversable in
-- a fixed order.
instance F.Foldable (IVar Trvrsbl) where
  foldr fn zer (IVar lv) =
    case unsafeDupablePerformIO$ readIORef (state lv) of
      Just x  -> fn x zer
      Nothing -> zer

instance (Show a) => Show (IVar Frzn a) where
  show (IVar lv) =
    show $ unsafeDupablePerformIO $ readIORef (state lv)

-- | For convenience only; the user could define this.
instance Show a => Show (IVar Trvrsbl a) where
  show = show . castFrzn 

--------------------------------------

{-# INLINE new #-}
-- | A new IVar that starts out empty. 
new :: Par d s (IVar s a)
new = WrapPar$ fmap (IVar . WrapLVar) $
      newLV $ newIORef Nothing

{-# INLINE get #-}
-- | Read the value in a IVar.  The 'get' can only return when the
-- value has been written by a prior or concurrent @put@ to the same
-- IVar.
get :: IVar s a -> Par d s a
get (IVar (WrapLVar iv)) = WrapPar$ getLV iv globalThresh deltaThresh
  where globalThresh ref _ = readIORef ref    -- past threshold iff Just _
        deltaThresh  x     = return $ Just x  -- always past threshold

{-# INLINE put_ #-}
-- | Put a value into an IVar.  Multiple 'put's to the same IVar
-- are not allowed, and result in a runtime error, unless the values put happen to be @(==)@.
--         
-- This function is always at least strict up to WHNF in the element put.
put_ :: Eq a => IVar s a -> a -> Par d s ()
put_ (IVar (WrapLVar iv)) !x = WrapPar $ putLV iv putter
  where putter ref      = atomicModifyIORef ref update
        update (Just y) | x == y = (Just y, Nothing)
                        | otherwise = unsafePerformIO $
                            do n1 <- fmap hashStableName $ makeStableName x
                               n2 <- fmap hashStableName $ makeStableName y
                               throw (LV.ConflictingPutExn$ "Multiple puts to an IVar! (obj "++show n2++" was "++show n1++")")
        update Nothing  = (Just x, Just x)

-- | A specialized freezing operation for IVars that leaves the result in a handy format (`Maybe`).
freezeIVar :: IVar s a -> I.Par QuasiDet s (Maybe a)
freezeIVar (IVar (WrapLVar lv)) = WrapPar $ 
   do freezeLV lv
      getLV lv globalThresh deltaThresh
  where
    globalThresh _  False = return Nothing
    globalThresh ref True = fmap Just $ readIORef ref
    deltaThresh _ = return Nothing
    
-- | Unpack a frozen IVar (as produced by a generic `freeze` operation) as a more
-- palatable data structure.
fromIVar :: IVar Frzn a -> Maybe a
fromIVar (IVar lv) = unsafeDupablePerformIO $ readIORef (state lv)

{-# INLINE whenFull #-}
-- | Register a handler that fires when the IVar is filled, which, of course, only
--   happens once.
whenFull :: Maybe LI.HandlerPool -> IVar s a -> (a -> Par d s ()) -> Par d s ()
whenFull mh (IVar (WrapLVar lv)) fn = 
   WrapPar (LI.addHandler mh lv globalCB fn')
  where
    fn' x = return (Just (unWrapPar (fn x)))
    globalCB ref = do
      mx <- readIORef ref -- Snapshot
      case mx of
        Nothing -> return Nothing
        Just v  -> fn' v
  
--------------------------------------------------------------------------------

{-# INLINE spawn #-}
-- | A simple future represented as an IVar.  The result is fully evaluated before
-- the child computation returns.
spawn :: (Eq a, NFData a) => Par d s a -> Par d s (IVar s a)
spawn p  = do r <- new;  LV.fork (p >>= put r);   return r

{-# INLINE spawn_ #-}
-- | A version of `spawn` that uses only WHNF, rather than full `NFData`.
spawn_ :: Eq a => Par d s a -> Par d s (IVar s a)
spawn_ p = do r <- new;  LV.fork (p >>= put_ r);  return r

{-# INLINE spawnP #-}
spawnP :: (Eq a, NFData a) => a -> Par d s (IVar s a)
spawnP a = spawn (return a)

{-# INLINE put #-}
-- | Fill an `IVar`.
put :: (Eq a, NFData a) => IVar s a -> a -> Par d s ()
put v a = deepseq a (put_ v a)

#ifdef GENERIC_PAR
instance PC.ParFuture (Par d s) where
  type Future (Par d s) = IVar s
  type FutContents (Par d s) a = (Eq a)
  spawn_ = spawn_
  get = get

instance PC.ParIVar (Par d s) where
  fork = LV.fork  
  put_ = put_
  new = new

#endif

