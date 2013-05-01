{-# LANGUAGE BangPatterns #-}

module Data.LVar.PairScalable
       (
         IPair,
         newPair,
         putFst,
         putSnd,
         getFst,
         getSnd,
        ) where
import LVarTraceScalable
import Data.IORef

------------------------------------------------------------------------------
-- IPairs implemented on top of LVars:
------------------------------------------------------------------------------

type IPair a b = LVar (IORef (IVarContents a),
                       IORef (IVarContents b))

newPair :: Par (IPair a b)
newPair = newLV $
          do r1 <- newIORef (IVarContents Nothing)
             r2 <- newIORef (IVarContents Nothing)
             return (r1,r2)

putFst :: IPair a b -> a -> Par ()
putFst lv@(LVar (refFst, _) _) !elt = putLV lv putter
  where
    -- putter takes the whole pair as an argument, but ignore it and
    -- just deal with refFst
    putter _ =
      atomicModifyIORef refFst $ \x -> 
      case fromIVarContents x of
        Nothing -> (IVarContents (Just elt), ())
        Just _  -> error "multiple puts to first element of IPair"
        
putSnd :: IPair a b -> b -> Par ()
putSnd lv@(LVar (_, refSnd) _) !elt = putLV lv putter
  where
    -- putter takes the whole pair as an argument, but ignore it and
    -- just deal with refSnd
    putter _ =
      atomicModifyIORef refSnd $ \x -> 
      case fromIVarContents x of
        Nothing -> (IVarContents (Just elt), ())
        Just _  -> error "multiple puts to second element of IPair"

getFst :: IPair a b -> Par a
getFst iv@(LVar (ref1,_)  _) = getLV iv poll
 where
   poll = fmap fromIVarContents $ readIORef ref1

getSnd :: IPair a b -> Par b
getSnd iv@(LVar (_,ref2) _) = getLV iv poll
 where
   poll = fmap fromIVarContents $ readIORef ref2
