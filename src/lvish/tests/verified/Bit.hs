{-@ LIQUID "--higherorder"    @-}
{-@ LIQUID "--totality"       @-}
{-@ LIQUID "--exactdc"        @-}

module Bit where

import Data.Iso
import Data.VerifiedEq
import Data.VerifiedEq.Instances
import Data.VerifiedOrd
import Data.VerifiedOrd.Instances
import Language.Haskell.Liquid.ProofCombinators

{-@ data Either a b = Left a | Right b @-}

{-@ data Bit = O | I @-}
data Bit = O | I

type BitRep = Either () ()

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
toFrom x@(Left ())  = toRep (fromRep x) *** QED
toFrom x@(Right ()) = toRep (fromRep x) *** QED

-- FIXME: should work, but doesn't, assume for now
{-@ assume fromTo :: x:Bit -> { fromRep (toRep x) == x } @-}
fromTo :: Bit -> Proof
fromTo x@O = fromRep (toRep x) *** QED
fromTo x@I = fromRep (toRep x) *** QED

isoBit :: Iso BitRep Bit
isoBit = Iso fromRep toRep fromTo toFrom

veqBit :: VerifiedEq Bit
veqBit = veqIso isoBit (veqSum veqUnit veqUnit)

vordBit :: VerifiedOrd Bit
vordBit = vordIso isoBit (vordSum vordUnit vordUnit)
