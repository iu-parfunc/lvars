{-# LANGUAGE BangPatterns, MultiParamTypeClasses #-}

module Data.LVar.IVar (IVar, new, get, put, put_, spawn, spawn_, spawnP) where

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
