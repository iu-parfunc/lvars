{-# LANGUAGE BangPatterns, MultiParamTypeClasses, TypeFamilies, TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-} 
module Data.LVar.IVar
       (IVar, 
        new, get, put, put_,
        spawn, spawn_, spawnP,
        freezeIVar, fromIVar,
        Data.LVar.IVar.addHandler)
       where

import           Data.IORef
import           Control.DeepSeq
import           System.Mem.StableName (makeStableName, hashStableName)
import           System.IO.Unsafe      (unsafePerformIO, unsafeDupablePerformIO)

import           Data.Traversable (traverse)
import qualified Data.Traversable as T
import qualified Data.Foldable    as F

import           Control.LVish as LV 
import           Control.LVish.DeepFrz.Internal
import           Control.LVish.Internal as I
import           Control.LVish.SchedIdempotent (newLV, putLV, getLV, freezeLV)
import qualified Control.LVish.SchedIdempotent as LI 
import           Data.Traversable (traverse)
import           GHC.Prim (unsafeCoerce#)

import           Data.LVar.Generic

#ifdef USE_ABSTRACT_PAR
import qualified Control.Monad.Par.Class as PC
#endif

------------------------------------------------------------------------------
-- IVars implemented on top of (the idempotent implementation of) LVars
------------------------------------------------------------------------------
       
-- the global data for an IVar a is a reference to Maybe a, while deltas are
-- simply values of type a (taking the IVar from Nothing to Just):
newtype IVar s a = IVar (LVar s (IORef (Maybe a)) a)

instance Eq (IVar s a) where
  (==) (IVar lv1) (IVar lv2) = state lv1 == state lv2

instance LVarData1 IVar where  
  freeze :: IVar s a -> Par QuasiDet s (IVar Frzn a)
  freeze orig@(IVar (WrapLVar lv)) = WrapPar $ do
    freezeLV lv
    return (unsafeCoerce# orig)

  -- newBottom :: Par d s (IVar s a)
  newBottom = new

-- | DeepFrz is just a type-coercion.  No bits flipped at runtime:
instance DeepFrz a => DeepFrz (IVar s a) where
  type FrzType (IVar s a) = IVar Frzn a 
  frz = unsafeCoerceLVar

--  traverseSnap f (IVarSnap m) = fmap IVarSnap $ traverse f m

-- instance T.Traversable (IVar Frzn) where
-- FINISHME 
  
-- instance Functor (IVar Frzn) where
  
instance F.Foldable (IVar Trvrsbl) where
  foldr fn zer (IVar lv) =
    case unsafeDupablePerformIO$ readIORef (state lv) of
      Just x  -> fn x zer
      Nothing -> zer 
      

--------------------------------------

{-# INLINE new #-}
new :: Par d s (IVar s a)
new = WrapPar$ fmap (IVar . WrapLVar) $
      newLV $ newIORef Nothing

{-# INLINE get #-}
-- | read the value in a @IVar@.  The 'get' can only return when the
-- value has been written by a prior or concurrent @put@ to the same
-- @IVar@.
get :: IVar s a -> Par d s a
get (IVar (WrapLVar iv)) = WrapPar$ getLV iv globalThresh deltaThresh
  where globalThresh ref _ = readIORef ref    -- past threshold iff Jusbt _
        deltaThresh  x     = return $ Just x  -- always past threshold

{-# INLINE put_ #-}
-- | put a value into a @IVar@.  Multiple 'put's to the same @IVar@
-- are not allowed, and result in a runtime error.
--         
-- Strict up to WHNF in the element put.
put_ :: Eq a => IVar s a -> a -> Par d s ()
put_ (IVar (WrapLVar iv)) !x = WrapPar $ putLV iv putter
  where putter ref      = atomicModifyIORef ref update
        update (Just y) | x == y = (Just y, Nothing)
                        | otherwise = unsafePerformIO $
                            do n1 <- fmap hashStableName $ makeStableName x
                               n2 <- fmap hashStableName $ makeStableName y
                               error$ "Multiple puts to an IVar! (obj "++show n2++" was "++show n1++")"
        update Nothing  = (Just x, Just x)

-- FIXME: documentation:
freezeIVar :: IVar s a -> LV.Par QuasiDet s (Maybe a)
freezeIVar (IVar lv) = liftIO $ readIORef (state lv)

-- 
fromIVar :: IVar Frzn a -> Maybe a
fromIVar (IVar lv) = unsafeDupablePerformIO $ readIORef (state lv)

{-# INLINE addHandler #-}
addHandler :: Maybe HandlerPool -> IVar s elt -> (elt -> Par d s ()) -> Par d s ()
addHandler mh (IVar (WrapLVar lv)) fn = 
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
spawn :: (Eq a, NFData a) => Par d s a -> Par d s (IVar s a)
spawn p  = do r <- new;  fork (p >>= put r);   return r

{-# INLINE spawn_ #-}
spawn_ :: Eq a => Par d s a -> Par d s (IVar s a)
spawn_ p = do r <- new;  fork (p >>= put_ r);  return r

{-# INLINE spawnP #-}
spawnP :: (Eq a, NFData a) => a -> Par d s (IVar s a)
spawnP a = spawn (return a)

{-# INLINE put #-}
put :: (Eq a, NFData a) => IVar s a -> a -> Par d s ()
put v a = deepseq a (put_ v a)

#ifdef USE_ABSTRACT_PAR
  -- MIN_VERSION_abstract_par(0,4,0)
#warning "Using the latest version of abstract par to activate ParFuture/IVar instances."
instance PC.ParFuture (IVar s) (Par d s) where
  spawn_ = spawn_
  get = get

instance PC.ParIVar (IVar s) (Par d s) where
  fork = fork  
  put_ = put_
  new = new
#endif

