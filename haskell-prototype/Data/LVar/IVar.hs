{-# LANGUAGE BangPatterns, MultiParamTypeClasses, TypeFamilies #-}

module Data.LVar.IVar
--       (IVar, new, get, put, put_, spawn, spawn_, spawnP, freezeIVar)
       where

import           Data.IORef
import           Control.DeepSeq
import qualified Control.Monad.Par.Class as PC
import           System.Mem.StableName (makeStableName, hashStableName)
import           System.IO.Unsafe      (unsafePerformIO)

import           Control.LVish

------------------------------------------------------------------------------
-- IVars implemented on top of (the idempotent implementation of) LVars
------------------------------------------------------------------------------
       
-- the global data for an IVar a is a reference to Maybe a, while deltas are
-- simply values of type a (taking the IVar from Nothing to Just):
newtype IVar a = IVar (LVar (IORef (Maybe a)) a)

instance Eq (IVar a) where
  (==) (IVar lv1) (IVar lv2) = state lv1 == state lv2

instance LVarData1 IVar where
  type Snapshot IVar a = Maybe a
  freeze    = freezeIVar
  newBottom = new

test = do
  iv1 <- newBottom :: Par (IVar (IVar String))
  iv2 <- newBottom
  put_ iv1 iv2
  put_ iv2 "hello"
  IVarSnap m <- freeze iv1
  return m

new :: Par (IVar a)
new = fmap IVar $ newLV $ newIORef Nothing

-- | read the value in a @IVar@.  The 'get' can only return when the
-- value has been written by a prior or concurrent @put@ to the same
-- @IVar@.
get :: IVar a -> Par a
get (IVar iv) = getLV iv globalThresh deltaThresh
  where globalThresh ref _ = readIORef ref    -- past threshold iff Jusbt _
        deltaThresh  x     = return $ Just x  -- always past threshold
        
-- | put a value into a @IVar@.  Multiple 'put's to the same @IVar@
-- are not allowed, and result in a runtime error.
--         
-- Strict up to WHNF in the element put.
put_ :: Eq a => IVar a -> a -> Par ()
put_ (IVar iv) !x = putLV iv putter
  where putter ref      = atomicModifyIORef ref update

        update (Just y) | x == y = (Just y, Just y)
                        | otherwise = unsafePerformIO $
                            do n1 <- fmap hashStableName $ makeStableName x
                               n2 <- fmap hashStableName $ makeStableName y
                               error$ "Multiple puts to an IVar! (obj "++show n2++" was "++show n1++")"
        update Nothing  = (Just x, Just x)


freezeIVar :: IVar a -> Par (Maybe a)
freezeIVar (IVar lv) =
  do freezeLV lv
     getLV lv globalThresh deltaThresh
  where
    globalThresh _  False = return Nothing
    globalThresh ref True = fmap Just $ readIORef ref
    deltaThresh _ = return Nothing

--------------------------------------------------------------------------------

spawn :: (Eq a, NFData a) => Par a -> Par (IVar a)
spawn p  = do r <- new;  fork (p >>= put r);   return r
              
spawn_ :: Eq a => Par a -> Par (IVar a)
spawn_ p = do r <- new;  fork (p >>= put_ r);  return r

spawnP :: (Eq a, NFData a) => a -> Par (IVar a)
spawnP a = spawn (return a)

put :: (Eq a, NFData a) => IVar a -> a -> Par ()
put v a = deepseq a (put_ v a)

{-
instance PC.ParFuture IVar Par where
  spawn_ = spawn_
  get = get

instance PC.ParIVar IVar Par where
  fork = fork  
  put_ = put_
  new = new
-}
