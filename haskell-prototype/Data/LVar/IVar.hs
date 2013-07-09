{-# LANGUAGE BangPatterns, MultiParamTypeClasses, TypeFamilies, TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE CPP #-} 
module Data.LVar.IVar
       (IVar, Snapshot(IVarSnap),
        new, get, put, put_,
        spawn, spawn_, spawnP,
        freezeIVar, addHandler)
       where

import           Data.IORef
import           Control.DeepSeq
import qualified Control.Monad.Par.Class as PC
import           System.Mem.StableName (makeStableName, hashStableName)
import           System.IO.Unsafe      (unsafePerformIO)

import           Data.Traversable (traverse)

import           Control.LVish as LV
import           Control.LVish.Internal as I
import           Control.LVish.SchedIdempotent (newLV, putLV, getLV, freezeLV)
import qualified Control.LVish.SchedIdempotent as LI 
import           Data.Traversable (traverse)

------------------------------------------------------------------------------
-- IVars implemented on top of (the idempotent implementation of) LVars
------------------------------------------------------------------------------
       
-- the global data for an IVar a is a reference to Maybe a, while deltas are
-- simply values of type a (taking the IVar from Nothing to Just):
newtype IVar s a = IVar (LVar s (IORef (Maybe a)) a)

instance Eq (IVar s a) where
  (==) (IVar lv1) (IVar lv2) = state lv1 == state lv2

instance LVarData1 IVar where
  -- type Snapshot IVar a = Maybe a
  newtype Snapshot IVar a = IVarSnap (Maybe a)
    deriving (Show,Ord,Read,Eq)
  
  freeze :: IVar s a -> Par QuasiDet s (Snapshot IVar a)
  freeze = unsafeConvert . fmap IVarSnap . freezeIVar

--newBottom :: forall (d :: Determinism) s1 a. Par d s1 (IVar s a)
  newBottom :: Par d s (IVar s a)
  newBottom = new
  
  traverseSnap f (IVarSnap m) = fmap IVarSnap $ traverse f m
  

unSnap :: Snapshot IVar a -> Maybe a
unSnap (IVarSnap m) = m

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


freezeIVar :: IVar s a -> LV.Par QuasiDet s (Maybe a)
freezeIVar (IVar (WrapLVar lv)) = WrapPar$ 
  do freezeLV lv
     getLV lv globalThresh deltaThresh
  where
    globalThresh _  False = return Nothing
    globalThresh ref True = fmap Just $ readIORef ref
    deltaThresh _ = return Nothing

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

#ifdef VERSION_abstract_par
#if 1
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
#endif

--------------------------------------------------------------------------------
-- IVar specific DeepFreeze instances:
--------------------------------------------------------------------------------


-- Teach it how to freeze WITHOUT the annoying snapshot constructor:
instance DeepFreeze (IVar s a) (Maybe a) where
  type Session (IVar s a) = s 
  deepFreeze iv = 
      do IVarSnap m <- freeze iv
         return m

instance DeepFreeze (IVar s a) b =>
         DeepFreeze (IVar s (IVar s a)) (Maybe b)
  where
    type Session (IVar s (IVar s a)) = s 
    deepFreeze from = do
      x <- freezeIVar from       -- :: Par QuasiDet s (Maybe (IVar s a))
      y <- traverse deepFreeze x -- :: Par QuasiDet s (Maybe b)
      return y

{-
-- [2013.07.06] Good, this errors post refactoring:
test :: IO Int
test = runParIO $ do
  v <- new
  logStrLn $ "First, put to the lvar..."
  put_ v 3
  logStrLn $ show $ unsafePerformIO$  runParIO $
    do
       (x::Maybe Int) <- deepFreeze v
       return x
  get v
-}
