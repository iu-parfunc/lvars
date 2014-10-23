{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

-- | Just for demonstration purposes.  It's probably simpler to use a pair of IVars.

module Data.LVar.Pair (
  IPair, newPair, putFst, putSnd, getFst, getSnd
  ) where

import Data.IORef
import Control.Exception (throw)
import Control.LVish
import Control.LVish.Internal
import Internal.Control.LVish.SchedIdempotent (newLV, putLV, getLV)
import qualified Internal.Control.LVish.SchedIdempotent as L
import           Data.LVar.Generic

------------------------------------------------------------------------------
-- IPairs implemented on top of (the idempotent implementation of) LVars:
------------------------------------------------------------------------------
       
type IPair s a b = LVar s (IORef (Maybe a), IORef (Maybe b)) (Either a b)

-- This can't be an intstance of LVarData1... we need LVarData2.

newPair :: Par e s (IPair s a b)
newPair = WrapPar $ fmap WrapLVar $ newLV $ do
  r1 <- newIORef Nothing
  r2 <- newIORef Nothing
  return (r1, r2)
  
putFst :: HasPut e => IPair s a b -> a -> Par e s ()
putFst (WrapLVar lv) !elt = WrapPar $ putLV lv putter
  where putter (r1, _)  = atomicModifyIORef r1 update
        update (Just _) = throw$ ConflictingPutExn$ "Multiple puts to first element of an IPair!"
        update Nothing  = (Just elt, Just $ Left elt)
        
putSnd :: HasPut e => IPair s a b -> b -> Par e s ()
putSnd (WrapLVar lv) !elt = WrapPar $ putLV lv putter
  where putter (_, r2)  = atomicModifyIORef r2 update
        update (Just _) = throw$ ConflictingPutExn$ "Multiple puts to second element of an IPair!"
        update Nothing  = (Just elt, Just $ Right elt) 
        
getFst :: HasGet e => IPair s a b -> Par e s a 
getFst (WrapLVar lv) = WrapPar $ getLV lv globalThresh deltaThresh
  where globalThresh (r1, _) _ = readIORef r1
        deltaThresh (Left x)   = return $ Just x
        deltaThresh (Right _)  = return Nothing
        
getSnd :: HasGet e => IPair s a b -> Par e s b 
getSnd (WrapLVar lv) = WrapPar $ getLV lv globalThresh deltaThresh
  where globalThresh (_, r2) _ = readIORef r2
        deltaThresh (Left _)   = return Nothing        
        deltaThresh (Right x)  = return $ Just x

-- TODO: LVarData2 instance??

