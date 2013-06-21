module Data.LVar.IVarIdem (IVar, new, get, put) where

import LVarIdempotent
import Data.IORef

------------------------------------------------------------------------------
-- IVars implemented on top of (the idempotent implementation of) LVars
------------------------------------------------------------------------------
       
-- the global data for an IVar a is a reference to Maybe a, while deltas are
-- simply values of type a (taking the IVar from Nothing to Just):
type IVar a = LVar (IORef (Maybe a)) a

new :: Par (IVar a)
new = newLV $ newIORef Nothing

-- | read the value in a @IVar@.  The 'get' can only return when the
-- value has been written by a prior or concurrent @put@ to the same
-- @IVar@.
get :: IVar a -> Par a
get iv = getLV iv globalThresh deltaThresh
  where globalThresh ref _ = readIORef ref    -- past threshold iff Jusbt _
        deltaThresh  x     = return $ Just x  -- always past threshold
        
-- | put a value into a @IVar@.  Multiple 'put's to the same @IVar@
-- are not allowed, and result in a runtime error.
put :: IVar a -> a -> Par ()        
put iv x = putLV iv putter
  where putter ref      = atomicModifyIORef ref update
        update (Just _) = error "Multiple puts to an IVar!"
        update Nothing  = (Just x, Just x)