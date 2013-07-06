{-# LANGUAGE BangPatterns #-}

module Data.LVar.Pair (
  IPair, newPair, putFst, putSnd, getFst, getSnd
  ) where

import Control.LVish
import Control.LVish.SchedIdempotent (newLV, putLV, getLV)
import Data.IORef

------------------------------------------------------------------------------
-- IPairs implemented on top of (the idempotent implementation of) LVars:
------------------------------------------------------------------------------
       
type IPair a b = LVar (IORef (Maybe a), IORef (Maybe b)) (Either a b)

newPair :: Par (IPair a b)
newPair = newLV $ do
  r1 <- newIORef Nothing
  r2 <- newIORef Nothing
  return (r1, r2)
  
putFst :: IPair a b -> a -> Par ()
putFst lv !elt = putLV lv putter
  where putter (r1, _)  = atomicModifyIORef r1 update
        update (Just _) = error "Multiple puts to first element of an IPair!"
        update Nothing  = (Just elt, Just $ Left elt)
        
putSnd :: IPair a b -> b -> Par ()
putSnd lv !elt = putLV lv putter
  where putter (_, r2)  = atomicModifyIORef r2 update
        update (Just _) = error "Multiple puts to second element of an IPair!"
        update Nothing  = (Just elt, Just $ Right elt) 
        
getFst :: IPair a b -> Par a 
getFst lv = getLV lv globalThresh deltaThresh
  where globalThresh (r1, _) _ = readIORef r1
        deltaThresh (Left x)   = return $ Just x
        deltaThresh (Right _)  = return Nothing
        
getSnd :: IPair a b -> Par b 
getSnd lv = getLV lv globalThresh deltaThresh
  where globalThresh (_, r2) _ = readIORef r2
        deltaThresh (Left _)   = return Nothing        
        deltaThresh (Right x)  = return $ Just x

