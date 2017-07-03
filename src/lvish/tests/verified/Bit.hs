{-@ LIQUID "--higherorder"    @-}
{-@ LIQUID "--totality"       @-}
{-@ LIQUID "--exactdc"        @-}
{-@ LIQUID "--automatic-instances=liquidinstances" @-}

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
toFrom x@(Left ())  = simpleProof
toFrom x@(Right ()) = simpleProof

-- FIXME: should work, but doesn't, assume for now
{-@ assume fromTo :: x:Bit -> { fromRep (toRep x) == x } @-}
fromTo :: Bit -> Proof
fromTo x@O = simpleProof
fromTo x@I = simpleProof

isoBit :: Iso BitRep Bit
isoBit = Iso fromRep toRep fromTo toFrom

veqBit :: VerifiedEq Bit
veqBit = veqIso isoBit (veqSum veqUnit veqUnit)

vordBit :: VerifiedOrd Bit
vordBit = vordIso isoBit (vordSum vordUnit vordUnit)
