{-# LANGUAGE BangPatterns #-}

module Data.LVar.Pair (
  IPair, newPair, putFst, putSnd, getFst, getSnd
  ) where

import Data.IORef
import Control.LVish
import Control.LVish.Internal
import Control.LVish.SchedIdempotent (newLV, putLV, getLV)
import qualified Control.LVish.SchedIdempotent as L

------------------------------------------------------------------------------
-- IPairs implemented on top of (the idempotent implementation of) LVars:
------------------------------------------------------------------------------
       
type IPair s a b = LVar s (IORef (Maybe a), IORef (Maybe b)) (Either a b)

-- This can't be an intstance of LVarData1... we need LVarData2.

newPair :: Par d s (IPair s a b)
newPair = WrapPar $ fmap WrapLVar $ newLV $ do
  r1 <- newIORef Nothing
  r2 <- newIORef Nothing
  return (r1, r2)
  
putFst :: IPair s a b -> a -> Par d s ()
putFst (WrapLVar lv) !elt = WrapPar $ putLV lv putter
  where putter (r1, _)  = atomicModifyIORef r1 update
        update (Just _) = error "Multiple puts to first element of an IPair!"
        update Nothing  = (Just elt, Just $ Left elt)
        
putSnd :: IPair s a b -> b -> Par d s ()
putSnd (WrapLVar lv) !elt = WrapPar $ putLV lv putter
  where putter (_, r2)  = atomicModifyIORef r2 update
        update (Just _) = error "Multiple puts to second element of an IPair!"
        update Nothing  = (Just elt, Just $ Right elt) 
        
getFst :: IPair s a b -> Par d s a 
getFst (WrapLVar lv) = WrapPar $ getLV lv globalThresh deltaThresh
  where globalThresh (r1, _) _ = readIORef r1
        deltaThresh (Left x)   = return $ Just x
        deltaThresh (Right _)  = return Nothing
        
getSnd :: IPair s a b -> Par d s b 
getSnd (WrapLVar lv) = WrapPar $ getLV lv globalThresh deltaThresh
  where globalThresh (_, r2) _ = readIORef r2
        deltaThresh (Left _)   = return Nothing        
        deltaThresh (Right x)  = return $ Just x

