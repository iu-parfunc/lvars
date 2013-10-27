{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# LANGUAGE GADTs #-}

-- |
--  This file provides a basic capability for parallel in-place modification of
-- (disjoint) partitions of an array.  It allows a `Par` computation to carry an
-- implicit vector in the background, which allows mutation via arbitrary `ST`
-- computations.
--
-- This module does NOT provide a monad-transformer.  Rather, it is a ROOT for a
-- monad-transformer stack, and is hard-wired to use the "Control.LVish" version of
-- the `Par` monad underneath.

module Control.LVish.ParVec
       -- (
       --   -- * The monad transformer: a dischargable effect
       --   ParVec, runParVec,

       --   -- * An alternate fork operation 
       --   forkWithVec,

       --   -- * Accessing the threaded Vector state
       --   getVec, initVec,
         
       --   -- * Working with ST and other lifts
       --   liftST, liftPar
       -- -- , dropST
       -- )
       where

import Control.Monad
import Control.Monad.IO.Class
import qualified Control.Monad.Trans as T
import qualified Control.Monad.Trans.State.Strict as S
import Control.Monad.ST        (ST)
import Control.Monad.ST.Unsafe (unsafeSTToIO, unsafeIOToST)
import Control.Monad.Trans (lift)

import Data.STRef
import Data.Vector.Mutable as MV
import Data.Vector       (freeze)
import Prelude hiding (read, length)
import System.IO.Unsafe (unsafePerformIO)

import GHC.Prim (RealWorld)

import Control.LVish (Par)
import qualified Control.Par.Class as PC
import qualified Control.LVish as LV
import qualified Data.LVar.IVar as IV -- ParFuture/ParIVar Instances.
-- import Data.LVar.IVar ()

unsafeCast :: ST s1 a -> ST s2 a
unsafeCast = unsafeIOToST . unsafeSTToIO

--------------------------------------------------------------------------------

class STSplittable (ty :: * -> * -> *) where
  type SplitIdx ty :: *

  splitST :: SplitIdx ty -> ty s a -> (ty s a, ty s a)

  unsafeCastSession :: ty s1 a -> ty s2 a

-- | Ways to split a vector.  For now we only allow splitting into two pieces at a
-- given index.  In the future, other ways of partitioning the set of elements may be
-- possible.
newtype VecSplit = SplitTwo Int

instance STSplittable MVector where
  type SplitIdx MVector = Int
  splitST mid vec = 
    let lvec = slice 0 mid vec
        rvec = slice mid (length vec - mid) vec
    in (lvec,rvec)

  unsafeCastSession = error "FINISHME - STVector unsafeCastSession"

-- data STTup2 s (a,b) = (s ~ Session b, s ~ Session a) => STTup2 !a !b

type family Fst ty :: *
type instance Fst (a,b) = a

type family Snd ty :: *
type instance Snd (a,b) = b

-- data STTup2 s tup = (s ~ Session (Fst tup), s ~ Session (Snd tup)) => STTup2 !(Fst tup) !(Snd tup)
data STTup2 s tup = STTup2 !(Fst tup) !(Snd tup)
-- I haven't figured out how to get this to work with raw, naked tuples yet.  So for
-- now, `STTup2`.

instance (Show (Fst tup), Show (Snd tup)) => Show (STTup2 s tup) where
  show (STTup2 a b) = "("++show a++","++show b++")"

test0 :: STTup2 s (Char,String)
test0 = STTup2 'i' "hi"


-- instance (STSplittable (Fst tup), STSplittable (Snd tup)) => STSplittable STTup2 where
--   type SplitIdx STTup2 = (SplitIdx (Fst tup), SplitIdx (Snd tup))

  -- splitST (spltA,spltB) (STTup2 a b) = 
  --   let (a',a'') = splitST spltA a 
  --       (b',b'') = splitST spltB b
  --   in ((STTup2 a' b'), (STTup2 a'' b''))
  -- unsafeCastSession = error "FINISHME - STTup2 unsafeCastSession"

{-

-- TEST:
-- type Tup s = (STVector s Int, STVector s Float)
type Tup s = STTup2 s (STVector s Int) (STVector s Float)
type Foo = Session (Tup RealWorld)
-- Make sure we can "roundtrip" and get out the right thing:
_ = (undefined :: RealWorld) :: Foo
-}

{-

--------------------------------------------------------------------------------

-- | The ParVec monad.  It uses the StateT monad transformer to layer
-- a state of type (STVector s elt) on top of an inner monad, `Control.LVish.Par`.
--
-- It, alas, has many type parameters.  `s1` and `elt` are for the `STVector`
-- computation (session and element type respectively).  `det` and `s2` are for the
-- underlying
-- 
-- Its final parameter, 'ans', is the result of running the entire computation, after
-- which the vector is no longer accessible.
newtype ParVec stState det s2 ans =
        ParVec ((S.StateT stState (Par det s2)) ans)
 deriving (Monad, Functor)

-- | @runParVec@ discharges the extra state effect leaving the the underlying par
-- Computation -- just like `runStateT`.  Here, using the standard trick runParVec has
-- a rank-2 type, with a phantom type 's'
runParVec :: forall stt s2 det ans .
             stt 
             -> (forall s1 . (Session stt ~ s1) =>
                             ParVec stt det s2 ans)
             -> Par det s2 ans
-- Here we're just using the ParVec value constructor tag to
-- destructure the argument to runParVec.  The 'st' in (ParVec st) is
-- of the type ((S.StateT (STVector s elt) ParIO) a).  The
-- unsafePerformIO lets us get the needed 'a' out of an IO
-- computation.
runParVec initVal (ParVec st) = unsafePerformIO io
 where
   -- Create a new mutable vector of length 0 and do a runStateT with
   -- it, getting back a monadic value of type ParIO, which we then
   -- run, getting a value and a final state.  We keep the value and
   -- throw away the state.
   io = do 
           let xm :: Par det s2 (ans, stt)
               xm = S.runStateT st initVal
               xm' = xm >>= (return . fst)
           return xm'

-- | getVec is a `ParVec` computation that results in the current
-- value of the state, which is of type 'STVector s elt'.
reify :: ParVec stt det s2 stt
reify = ParVec S.get

-- | initVec creates a new mutable vector and returns a `ParVec`
-- computation with that new mutable vector's state as its state.
install :: stt -> ParVec stt det s2 ()
install val = ParVec (S.put val)



-- | @forkWithVec@ takes a split point and two ParVec computations.  It
-- gets the state of the current computation, which is a vector, and
-- then divides up that state between the two other computations.
-- Writes to those two computations actually mutate the original
-- vector.
--
-- @forkWithVec@ is a fork-join construct, rather than a one-sided fork such as
-- `fork`.
forkWithVec :: forall a b s2 det stt stt1 stt2 .
               (Eq a, STSplittable stt) =>
               (SplitIdx stt)
            -> (forall sl . ParVec (WithSession sl stt) det s2 a) -- ^ Left child computation.
            -> (forall sr . ParVec (WithSession sr stt) det s2 b) -- ^ Right child computation.
            -> ParVec stt det s2 (a,b)

-- forkWithVec :: forall a b s2 det stt stt1 stt2 .
--                (Eq a, STSplittable stt, STSplittable stt1, STSplittable stt2,
--                       StripSession stt1 ~ StripSession stt,
--                       StripSession stt2 ~ StripSession stt) =>
--                (SplitIdx stt)
--             -> (forall sl . (sl ~ Session stt1) =>
--                             ParVec stt1 det s2 a) -- ^ Left child computation.
--             -> (forall sr . (sr ~ Session stt2) =>
--                             ParVec stt2 det s2 b) -- ^ Right child computation.
--             -> ParVec stt det s2 (a,b)
forkWithVec spltidx (ParVec lef) (ParVec rig) = ParVec $ do
  snap <- S.get
  let slice1, slice2 :: stt
      (slice1,slice2) = splitST spltidx snap
      
  -- lv <- lift$ IV.spawn_$ S.evalStateT lef left
  lv <- lift$ IV.spawn_$ S.evalStateT lef (unsafeCastSession slice1)
  -- lv <- lift$ IV.spawn_$ S.evalStateT lef undefined
  
  -- S.put right
  let -- rig' :: S.StateT (WithSession (Session stt) stt) (Par det s2) b
      -- rig' :: S.StateT stt (Par det s2) b
      -- rig' = rig
      
  -- rx <- rig                     -- Do the R one on this thread.
  -- lx <- lift$ IV.get lv         -- Wait for the forked thread to finish.
  -- S.put snap                    -- Put the whole state back in place.
  -- return (lx,rx)
  undefined

-}


{-

-- | Allow `ST` computations inside `ParVec` computations.
--   This operation has some overhead. 
liftST :: ST s1 a -> ParVec s1 elt det s2 a
liftST st =
  seq thunk (return thunk)
 where
   -- WARNING: this requires locking on EACH unsafePerformIO.
   -- This is inefficient IF the underlying 'par' actually would
   -- have the capability to peform IO more efficiently.
   -- But we can't assume that.
   thunk = unsafePerformIO io
   io    = unsafeSTToIO st 

-- | Lift an ordinary `Par` computation into `VecPar`.
liftPar :: Par d s a -> ParVec s1 elt d s a 
liftPar m = ParVec (lift m)

-- -- | Rather than lifting ST into the ParVec, drop it into the underlying `par` monad as IO.
-- --   In some situations this might be more efficient.   
-- dropST :: MonadIO (Par det s2) =>
--           ST s a -> ParVec s1 elt det s2 a
-- dropST = ParVec . liftIO . unsafeSTToIO  

instance MonadIO (Par det s2) => MonadIO (ParVec s1 elt det s2) where
  liftIO io = ParVec (liftIO io)

{-

instance PC.ParIVar par =>
-- instance PC.ParFuture par =>
         PC.ParFuture (ParVec s elt par) where
  type Future      (ParVec s elt par)   = PC.Future par
  type FutContents (ParVec s elt par) a = PC.FutContents par a
  spawn_ (ParVec task) = ParVec $ 
    do iv <- lift $ PC.new
       lift $ PC.fork $ do
           (res,_) <- S.runStateT task
                      (error "spawn_: This child thread does not have permission to touch the array!")
           PC.put_ iv res
       return iv
  get iv = ParVec $ lift $ PC.get iv


instance PC.ParIVar par =>
         PC.ParIVar (ParVec s elt par) where
  fork (ParVec task) = ParVec $ 
    lift $ PC.fork $ do
      (res,_) <- S.runStateT task
                 (error "fork: This child thread does not have permission to touch the array!")
      return res
  new       = ParVec$ lift PC.new
  put_ iv v = ParVec$ lift$ PC.put_ iv v
  
-}


--------------------------------------------------------------------------------
-- Tests and Scrap:
--------------------------------------------------------------------------------

-- Little tests:
t1 :: IO String
t1 = unsafeSTToIO p1

p1 :: ST s String
p1 = do
  r <- newSTRef "hi"
  writeSTRef r "hello"
  readSTRef r

-}

