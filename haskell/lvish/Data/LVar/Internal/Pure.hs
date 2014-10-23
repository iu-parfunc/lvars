{-# LANGUAGE Unsafe #-}

{-# LANGUAGE DataKinds, BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE InstanceSigs, MagicHash #-}

{-|

This is /not/ a datatype for the end-user.

Rather, this module is for building /new/ LVar types in a comparatively easy way: by
putting a pure value in a mutable container, and defining a @put@ operation as a pure
function.

The data structure implementor who uses this module must guarantee
that their @put@ operation computes a /least upper bound/, ensuring
that the set of states that their LVar type can take on form a
join-semilattice (<http://en.wikipedia.org/wiki/Semilattice>).

-}

module Data.LVar.Internal.Pure
       ( PureLVar(..),
         newPureLVar, putPureLVar,

         waitPureLVar, freezePureLVar, fromPureLVar, 
         getPureLVar, unsafeGetPureLVar,

         -- * Verifying lattice structure
         verifyFiniteJoin, verifyFiniteGet
       ) where

import Control.LVish
import Control.LVish.DeepFrz.Internal
import Control.LVish.Internal
import Data.IORef
import qualified Data.Set as S
import qualified Internal.Control.LVish.SchedIdempotent as LI 
import Algebra.Lattice
import GHC.Prim (unsafeCoerce#)
import System.IO.Unsafe (unsafePerformIO, unsafeDupablePerformIO)
--------------------------------------------------------------------------------

-- | An LVar which consists merely of an immutable, pure value inside a mutable box.
newtype PureLVar s t = PureLVar (LVar s (IORef t) t)

instance Show a => Show (PureLVar Frzn a) where
  show (PureLVar lv) = show$ unsafePerformIO$ readIORef$ state lv

-- data PureLVar s t = BoundedJoinSemiLattice t => PureLVar (LVar s (IORef t) t)

{-# INLINE newPureLVar #-}
{-# INLINE putPureLVar #-}
{-# INLINE getPureLVar #-}
{-# INLINE waitPureLVar #-}
{-# INLINE freezePureLVar #-}

-- | Takes a finite set of states and a join operation (e.g., for an
-- instance of JoinSemiLattice) and returns an error message if the
-- join-semilattice properties don't hold.
verifyFiniteJoin :: (Eq a, Show a) => [a] -> (a -> a -> a) -> Maybe String
verifyFiniteJoin allStates join =
  case (isCommutative, isAssociative, isIdempotent) of
    (hd : _ , _, _) -> Just $ "commutativity violated!: " ++ show hd
    (_ , hd : _, _) -> Just $ "associativity violated!: " ++ show hd
    (_ , _, hd : _) -> Just $ "idempotency violated!: " ++ show hd
    ([], [], []) -> Nothing
  where
    isCommutative = [(a, b) | a <- allStates, b <- allStates, a `join` b /= b `join` a]
    isAssociative = [(a, b, c) |
                     a <- allStates,
                     b <- allStates,
                     c <- allStates,
                     a `join` (b `join` c) /= (a `join` b) `join` c]
    isIdempotent = [a | a <- allStates, a `join` a /= a]

-- | Verify that a blocking get is monotone in just the right way.
--   This takes a designated bottom and top element.
verifyFiniteGet :: (Eq a, Show a, JoinSemiLattice a,
                    Eq b, Show b) =>
                   [a] -> (b,b) -> (a -> b) -> Maybe String
verifyFiniteGet allStates (bot,top) getter =
   case (botBefore, constAfter) of
     ((a,b):_, _) -> Just$ "violation! input "++ show a
                      ++" unblocked get, but larger input"++show b++" did not."
     (_, (a,b):_) -> Just$ "violation! value at "++ show a
                      ++" was non-bottom ("++show (getter a)
                      ++"), but then changed at "++show b++" ("++ show (getter b)++")"
     ([],[])      -> Nothing
  where
   botBefore = [ (a,b)
               | a <- allStates, b <- allStates
               , a `joinLeq` b,  getter b == bot
               , not (getter a == bot) ]
   constAfter = [ (a,b)
                | a <- allStates, b <- allStates
                , a `joinLeq` b
                , getter a /= bot
                , getter a /= getter b
                , getter b /= top -- It's ok to go to error.
                ]


-- | A new pure LVar populated with the provided initial state.
newPureLVar :: JoinSemiLattice t =>
               t -> Par e s (PureLVar s t)
newPureLVar st = WrapPar$ fmap (PureLVar . WrapLVar) $
                 LI.newLV $ newIORef st

-- | Blocks until the contents of `lv` are at or above one element of
-- `thrshSet`, then returns that one element.
getPureLVar :: (JoinSemiLattice t, Eq t, HasGet e) => PureLVar s t -> [t] -> Par e s t
getPureLVar (PureLVar (WrapLVar lv)) thrshSet =
  WrapPar$ LI.getLV lv globalThresh deltaThresh
  where globalThresh ref _ = do
          x <- readIORef ref
          logDbgLn_ 5 "  [Pure] Getting from a Pure LVar.. read ref."
          deltaThresh x
        deltaThresh x =
          return $ checkThresholds x thrshSet

-- | Returns the element of thrshSet that `currentState` is above, if
-- it exists.  (Assumes that there is only one such element!)
checkThresholds :: (JoinSemiLattice t, Eq t) => t -> [t] -> Maybe t
-- ARGH: This is inefficient IF the state is IN the threshold set.  In that case a
-- log(N) Set.member test should be enough, not a full O(N) traversal of all states
-- in the threshold set.
checkThresholds currentState thrshSet = case thrshSet of
  []               -> Nothing
  (thrsh : thrshs) -> if thrsh `joinLeq` currentState
                      then Just thrsh
                      else checkThresholds currentState thrshs

-- | A variant of getPureLVar that allows /equivalence classes/ of sets.
--   All equivalence classes in a threshold set must still be pairwise incompatible.
--
--   Further, as a result of running this get, you find out only which equivalence
--   class the current state is at or above, not exactly which state within that
--   equivalence class.
-- 
--   Finally, to make it easy to know WHICH set you got back, we allow each set to be
--   tagged with an arbitrary additional value.  For example, it may be useful to use
--   a unique `Int` for this purpose.
getPureLVarSets :: (JoinSemiLattice t, Eq t, Ord t, HasGet e) 
                => PureLVar s t -> [(b, S.Set t)] -> Par e s (b, S.Set t)
getPureLVarSets (PureLVar (WrapLVar lv)) thrshSets =
  WrapPar$ LI.getLV lv globalThresh deltaThresh
  where globalThresh ref _ = do
          x <- readIORef ref
          logDbgLn_ 5 "  [Pure] Getting from a Pure LVar.. read ref."
          deltaThresh x
        deltaThresh x =
          return $ checkThresholds2 x thrshSets

-- | Returns the element of thrshSet that `currentState` is above, if
-- it exists.  (Assumes that there is only one such element!)
checkThresholds2 :: (JoinSemiLattice t, Eq t) => t -> [(b,S.Set t)] -> Maybe (b,S.Set t)
checkThresholds2 currentState thrshSet = case thrshSet of
  []               -> Nothing
  (hd@(tag,eqset) : thrshs) -> if anySet eqset (`joinLeq` currentState)
                          then Just hd
                          else checkThresholds2 currentState thrshs

-- Huh, why is this not in Data.Set, analogous to Data.List.any
anySet :: S.Set a -> (a -> Bool) -> Bool
anySet st fn = 
  -- ARGH: this should short circuit.  Data.Set screws us up here by not giving us
  -- efficient iteration with control over recursion.
  S.foldl' (\ flg elm -> flg || fn elm) False st

-- | Like `getPureLVar` but uses a threshold function rather than an explicit set.
unsafeGetPureLVar :: (JoinSemiLattice t, Eq t, HasGet e) => PureLVar s t -> (t -> Bool) -> Par e s t
unsafeGetPureLVar (PureLVar (WrapLVar lv)) thrsh =
  WrapPar$ LI.getLV lv globalThresh deltaThresh
  where globalThresh ref _ = do
          x <- readIORef ref
          logDbgLn_ 5 "  [Pure] unsafeGetPureLVar: read the ref."
          deltaThresh x
        deltaThresh x =          
          return $! if thrsh x
                    then Just x
                    else Nothing

-- | Wait until the pure LVar has crossed a threshold and then unblock.  (In the
-- semantics, this is a singleton query set.)
waitPureLVar :: (JoinSemiLattice t, Eq t, HasGet e) =>
                PureLVar s t -> t -> Par e s ()
waitPureLVar (PureLVar (WrapLVar iv)) thrsh =
   WrapPar$ LI.getLV iv globalThresh deltaThresh
  where globalThresh ref _ = do
          x <- readIORef ref
          logDbgLn_ 5 "  [Pure] checking global thresh..."
          deltaThresh x
        deltaThresh x | thrsh `joinLeq` x = do logDbgLn_ 5 "  [Pure] Delta thresh met!"
                                               return $ Just ()
                      | otherwise         = do logDbgLn_ 5 "  [Pure] Check Delta thresh.. Not yet."
                                               return Nothing 

-- | Put a new value which will be joined with the old.
putPureLVar :: (JoinSemiLattice t, HasPut e) =>
               PureLVar s t -> t -> Par e s ()
putPureLVar (PureLVar (WrapLVar iv)) !new =
    WrapPar $ LI.putLV iv putter
  where
    -- Careful, this must be idempotent...
    putter !ref = do
      -- In some cases direct CAS would be better than atomicModifyIORef here.
      logDbgLn_ 5 "  [Pure] Putting to pure LVar.."
      atomicModifyIORef' ref $ \ oldval -> (join oldval new, ())
      -- We still publish the change for delta-thresh's to respond to:
      return $! Just $! new

-- | Freeze the pure LVar, returning its exact value.
--   Subsequent @put@s will raise an error.
freezePureLVar :: HasFreeze e => PureLVar s t -> Par e s t
freezePureLVar (PureLVar (WrapLVar lv)) = WrapPar$ 
  do LI.freezeLV lv
     LI.getLV lv globalThresh deltaThresh
  where
    globalThresh ref True = fmap Just $ readIORef ref
    globalThresh _  False = return Nothing
    deltaThresh  _        = return Nothing

-- | Read the exact contents of an already frozen PureLVar.
fromPureLVar :: PureLVar Frzn t -> t
fromPureLVar (PureLVar lv) =
  unsafeDupablePerformIO $ readIORef $ state lv


------------------------------------------------------------

-- | Physical identity, just as with `IORef`s.
instance Eq (PureLVar s v) where
  PureLVar lv1 == PureLVar lv2 = state lv1 == state lv2 

-- `PureLVar` values can be returned as the result of a
--  `runParThenFreeze`.  Hence they need a `DeepFrz` instance.
--  @DeepFrz@ is just a type-coercion.  No bits flipped at runtime.
instance DeepFrz a => DeepFrz (PureLVar s a) where
  -- We can't be sure that someone won't put an LVar value inside a
  -- PureLVar!  Therefore we have to apply FrzType recursively.
  type FrzType (PureLVar s a) = PureLVar Frzn (FrzType a)
  frz = unsafeCoerce#

-- FIXME: need an efficient way to extract the logger and access it from the callbacks.
-- Currently there is no good way to log from IO context:
logDbgLn_ :: Int -> String -> IO ()
logDbgLn_ _ _ = return ()
