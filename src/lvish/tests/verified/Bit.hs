{-@ LIQUID "--higherorder"    @-}
{-@ LIQUID "--totality"       @-}
{-@ LIQUID "--exactdc"        @-}
{-@ LIQUID "--prune-unsorted" @-}
{-# LANGUAGE BangPatterns     #-}

module Bit where

import Data.Iso
import Language.Haskell.Liquid.ProofCombinators

{-@ data Either a b = Left a | Right b @-}

{-@ data Bit = O | I @-}
data Bit = O | I
  deriving (Eq, Ord, Enum)

type Unit = ()

unit :: Unit
unit = ()

type BitRep = Either Unit Unit

{-@ axiomatize toRep @-}
toRep :: Bit -> BitRep
toRep O = Left ()
toRep I = Right ()

{-@ axiomatize fromRep @-}
fromRep :: BitRep -> Bit
fromRep (Left ())  = O
fromRep (Right ()) = I

{-@ toFrom :: x:BitRep -> { toRep (fromRep x) == x } @-}
toFrom :: BitRep -> Proof
toFrom x@(Left ())  = toRep (fromRep x) ==. toRep O ==. x *** QED
toFrom x@(Right ()) = toRep (fromRep x) ==. toRep I ==. x *** QED

{-@ fromTo :: x:Bit -> { fromRep (toRep x) == x } @-}
fromTo :: Bit -> Proof
fromTo x@O = fromRep (toRep x) ==. fromRep (toRep O) ==. fromRep (Left ()) ==. O ==. x *** QED
fromTo x@I = fromRep (toRep x) ==. fromRep (toRep I) ==. fromRep (Right ()) ==. I ==. x *** QED

isoBit :: Iso BitRep Bit
isoBit = Iso fromRep toRep fromTo toFrom
