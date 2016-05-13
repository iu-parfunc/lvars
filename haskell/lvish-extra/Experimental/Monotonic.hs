{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | EXPERIMENTAL

-- | A restricted sub-language for writing ONLY monotonic computations.

module Monotonic
       -- Control.LVish.Monotonic
       where

import Control.DeepSeq

import qualified Control.LVish as L
import qualified Data.LVar.IVar as I
import qualified Data.LVar.PureSet  as S

import Classes

import Data.Word
import Data.IORef
import qualified Data.Set as Set
--------------------------------------------------------------------------------


-- | A value used in a monotonic computation.
newtype Mono a = Mono a 

-- | A monotonic, parallel computation with side effects.
newtype MPar a = MPar { unMPar :: L.Par a }
  deriving (Monad )

-- We can't lift the raw LVar ops, since they are unsafe anyway!
-- putLV :: LVar a d -> (Mono a -> IO (Maybe d)) -> Par ()

--------------------------------------------------------------------------------
-- Lifted IVar ops:

-- Oops, wait, this won't work!  This only makes sense for counters, not ivars...
put_ :: Eq a => I.IVar a -> Mono a -> MPar ()
put_ i (Mono a) = MPar$ I.put_ i a

get :: I.IVar a -> MPar (Mono a)
get = MPar . fmap Mono . I.get 

----------------------------------------
-- Lifted Set ops:

putInSet :: Ord a => Mono a -> S.ISet a -> MPar ()
putInSet (Mono a) set = MPar $ S.insert a set

-- By using monotonic callbacks we can guarantee that premature freezes can be rerun
-- rather than resulting in errors.
-- withCallbacksThenFreeze :: Eq b => S.ISet a -> (Mono a -> MPar ()) -> MPar b -> MPar b
-- withCallbacksThenFreeze st cb initm = return undefined

setForeach :: Mono (Snapshot S.ISet a) -> (Mono a -> MPar ()) -> MPar ()
setForeach = undefined

-- Here we speculatively assume that it is safe to freeze and snapshot, but to be
-- careful we only run a monotonic computation on the snapshot.  Any side effects it
-- have must *increase* if it is passed a larger snapshot.  If there is a put AFTER
-- this freeze, the monotonic callback can simply be rerun.
speculateFrozen :: (LVishData1 f) =>
                   f a -> (Mono (Snapshot f a) -> MPar ()) -> L.Par ()
-- speculateFrozen :: (LVishData1 f, LVishData1 g) =>
--                    f a -> (Mono (Snapshot f a) -> MPar (g b)) -> L.Par (g b)
speculateFrozen = error "finishme -speculateFrozen"

-- FIXME: What prevents bad combinations such as a monotonically shrinking set with a
-- monotonically growing set?  


--------------------------------------------------------------------------------
-- Can we enable a limited form of monotonic math?
--
-- But that assumes the normal total-ordering for builtin types like Word...

add :: Mono Word -> Mono Word -> Mono Word
add (Mono a) (Mono b) = Mono (a + b)

mul :: Mono Word -> Mono Word -> Mono Word
mul (Mono a) (Mono b) = Mono (a * b)

-- This is only true for NON-negative numbers!
setSum :: Mono (Snapshot S.ISet Word) -> Mono Word
setSum = undefined

setSize :: Mono (Snapshot S.ISet Word) -> Mono Word
setSize (Mono s) = Mono $ fromIntegral $ Set.size s

mconst :: Num a => a -> Mono a
mconst = Mono

example :: L.Par ()
example = do
  s1 <- S.newEmptySet
  s2 <- S.newEmptySet
  mapM_ (\n -> L.fork $ S.insert n s1) [1..10::Word]
  -- sync here if desired...
  speculateFrozen s1 $ \ snap -> do
    setForeach snap  $ \ elem -> do
      putInSet (elem `mul` mconst 10) s2
  return ()

example2 :: L.Par Counter
example2 = do
  s1 <- S.newEmptySet
  sz <- newCounter
  sm <- newCounter
  mapM_ (\n -> L.fork $ S.insert n s1) [1..10::Word]
  -- sync here if desired...
  speculateFrozen s1 $ \ snap -> do
    setForeach snap  $ \ elem -> do
      setCounter sz (setSize snap)
      setCounter sm (setSum  snap)
  return sm


newtype Counter = Counter (L.LVar (IORef Word) ())
newCounter :: L.Par Counter
newCounter = undefined

setCounter :: Counter -> Mono Word -> MPar ()
setCounter = undefined
  
freezeCounter :: Counter -> L.Par Word
freezeCounter = undefined

