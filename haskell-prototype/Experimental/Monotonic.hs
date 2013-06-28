

-- | EXPERIMENTAL

-- | A restricted sub-language for writing ONLY monotonic computations.

module Monotonic
       -- Control.LVish.Monotonic
       where

import Control.DeepSeq

import qualified Control.LVish as L
import qualified Data.LVar.IVar as I
import qualified Data.LVar.Set  as S

import Classes

import Data.Set (Set)
--------------------------------------------------------------------------------


-- | A value used in a monotonic computation.
newtype Mono a = Mono a 

-- | A monotonic, parallel computation with side effects.
newtype MPar a = MPar { unMPar :: L.Par a }

-- We can't lift the raw LVar ops, since they are unsafe anyway!
-- putLV :: LVar a d -> (Mono a -> IO (Maybe d)) -> Par ()

--------------------------------------------------------------------------------
-- Lifted IVar ops:

put_ :: Eq a => I.IVar a -> Mono a -> MPar ()
put_ i (Mono a) = MPar$ I.put_ i a

get :: I.IVar a -> MPar a
get = MPar . I.get 

----------------------------------------
-- Lifted Set ops:

putInSet :: Ord a => Mono a -> S.ISet a -> MPar ()
putInSet (Mono a) set = MPar $ S.putInSet a set

-- By using monotonic callbacks we can guarantee that premature freezes can be rerun
-- rather than resulting in errors.
-- withCallbacksThenFreeze :: Eq b => S.ISet a -> (Mono a -> MPar ()) -> MPar b -> MPar b
-- withCallbacksThenFreeze st cb initm = return undefined


-- speculateFrozen :: S.ISet a -> (Mono (Set a) -> MPar (Mono b)) -> L.Par (Snapshot b)
speculateFrozen :: (LVishData1 f, LVishData1 g) =>
                   f a -> (Mono (Snapshot f a) -> MPar (g b)) -> L.Par (g b)
speculateFrozen = error "finishme -speculateFrozen"


--------------------------------------------------------------------------------
-- Can we enable a limited form of monotonic math?
-- But that assumes an ordering for builtin types like Int...

add :: Num a => Mono a -> Mono a -> Mono a
add (Mono a) (Mono b) = Mono (a + b)

mul :: Num a => Mono a -> Mono a -> Mono a
mul (Mono a) (Mono b) = Mono (a * b)

