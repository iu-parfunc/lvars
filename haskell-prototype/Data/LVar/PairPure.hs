{-# LANGUAGE BangPatterns #-}

module Data.LVar.PairPure
       (
         newPair,
         putFst,
         putSnd,
         getFst,
         getSnd, 
         ) where
import LVarTracePure

------------------------------------------------------------------------------
-- IPairs implemented on top of LVars:
------------------------------------------------------------------------------

type IPair a b = LVar (IVarContents a, IVarContents b)

newPair :: Par (IPair a b)
newPair = newLV (IVC Nothing,
                 IVC Nothing)

putFst :: IPair a b -> a -> Par ()
putFst lv !elt = putLV lv (IVC (Just elt), IVC Nothing)

putSnd :: IPair a b -> b -> Par ()
putSnd lv !elt = putLV lv (IVC Nothing, IVC (Just elt))

getFst :: IPair a b -> Par a
getFst lv = getLV lv test
 where
   test (IVC (Just x),_) = Just x
   test (IVC Nothing,_)  = Nothing

getSnd :: IPair a b -> Par b
getSnd lv = getLV lv test
 where
   test (_,IVC (Just x)) = Just x
   test (_,IVC Nothing)  = Nothing

