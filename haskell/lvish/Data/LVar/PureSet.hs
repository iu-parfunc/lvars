{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}

{-|

  This module provides sets that only grow.  It is based on the popular "Data.Set"
  balanced-tree representation of sets.  Thus scalability is /not/ good for this
  implementation.  However, there are some interoperability benefits.  For exmaple,
  after running a parallel computation with a set result, this module can produce a
  `Set` in /O(1)/ without copying, which may be useful downstream.

 -}

module Data.LVar.PureSet
       (
         -- * Basic operations
         ISet, 
         newEmptySet, newSet, newFromList,
         insert, waitElem, waitSize, 

         -- * Iteration and callbacks
         forEach, forEachHP, 

         -- * Quasi-deterministic operations
         freezeSetAfter, withCallbacksThenFreeze, freezeSet,
         fromISet,
         
         -- * Higher-level derived operations
         copy, traverseSet, traverseSet_, union, intersection,
         cartesianProd, cartesianProds, 

         -- * Alternate versions of derived ops that expose @HandlerPool@s they create
         traverseSetHP, traverseSetHP_, unionHP, intersectionHP,
         cartesianProdHP, cartesianProdsHP
       ) where

import           Control.Monad (void)
import           Control.Applicative ((<$>))
import           Data.IORef
import           Data.List (intersperse)
import qualified Data.Set as S
import qualified Data.LVar.IVar as IV
import qualified Data.Foldable as F
import           Data.LVar.Generic
import           Data.LVar.Generic.Internal (unsafeCoerceLVar)
import           Control.LVish as LV
import           Control.LVish.DeepFrz.Internal
import           Control.LVish.Internal as LI
import           Control.LVish.SchedIdempotent (newLV, putLV, getLV, freezeLV, freezeLVAfter)
import qualified Control.LVish.SchedIdempotent as L
import           System.IO.Unsafe (unsafeDupablePerformIO)
import Prelude hiding (insert)

------------------------------------------------------------------------------
-- ISets and setmap implemented on top of LVars:
------------------------------------------------------------------------------

-- | The set datatype itself.  Like all other LVars, it has an @s@ parameter (like
-- an `STRef`) in addition to the @a@ parameter that describes the type of elements
-- in the set.
--
-- Performance note: There is only /one/ mutable location in this implementation.  Thus
-- it is not a scalable implementation.
newtype ISet s a = ISet (LVar s (IORef (S.Set a)) a)

-- | Physical identity, just as with `IORef`s.
instance Eq (ISet s v) where
  ISet lv1 == ISet lv2 = state lv1 == state lv2 

-- | An `ISet` can be treated as a generic container LVar.  However, the polymorphic
-- operations are less useful than the monomorphic ones exposed by this module.
instance LVarData1 ISet where
  freeze orig@(ISet (WrapLVar lv)) = WrapPar$ do freezeLV lv; return (unsafeCoerceLVar orig)
  addHandler = forEachHP
  -- | We can do better than the default here; this is /O(1)/:
  sortFrzn (ISet lv) = AFoldable$ unsafeDupablePerformIO (readIORef (state lv))

-- | The `ISet`s in this module also have the special property that they support an
-- /O(1)/ freeze operation which immediately yields a `Foldable` container
-- (`snapFreeze`).
instance OrderedLVarData1 ISet where
  snapFreeze is = unsafeCoerceLVar <$> freeze is

-- As with all LVars, after freezing, map elements can be consumed. In
-- the case of this `ISet` implementation, it need only be `Frzn`, not
-- `Trvrsbl`.
instance F.Foldable (ISet Frzn) where
  foldr fn zer (ISet lv) =
    -- It's not changing at this point, no problem if duped:
    let set = unsafeDupablePerformIO (readIORef (state lv)) in
    F.foldr fn zer set 

-- Of course, the stronger `Trvrsbl` state is still fine for folding.
instance F.Foldable (ISet Trvrsbl) where
  foldr fn zer mp = F.foldr fn zer (castFrzn mp)

-- `ISet` values can be returned as the result of a
--  `runParThenFreeze`.  Hence they need a `DeepFrz` instance.
--  @DeepFrz@ is just a type-coercion.  No bits flipped at runtime.
instance DeepFrz a => DeepFrz (ISet s a) where
  type FrzType (ISet s a) = ISet Frzn (FrzType a)
  frz = unsafeCoerceLVar

instance (Show a) => Show (ISet Frzn a) where
  show (ISet lv) =
    let set = S.toList $ unsafeDupablePerformIO $ readIORef (state lv) in
    "{ISet: " ++
     (concat $ intersperse ", " $ map show set) ++ "}"

-- | For convenience; the user could define this.
instance Show a => Show (ISet Trvrsbl a) where
  show = show . castFrzn

-- | Create a new, empty, monotonically growing set.
newEmptySet :: Par d s (ISet s a)
newEmptySet = newSet S.empty

-- | Create a new set populated with initial elements.
newSet :: S.Set a -> Par d s (ISet s a)
newSet s = WrapPar$ fmap (ISet . WrapLVar) $ newLV$ newIORef s

-- | Create a new set drawing initial elements from an existing list.
newFromList :: Ord a => [a] -> Par d s (ISet s a)
newFromList ls = newSet (S.fromList ls)

-- (Todo: in production you might want even more ... like going from a Vector)

--------------------------------------------------------------------------------
-- Quasi-deterministic ops:
--------------------------------------------------------------------------------

-- | Freeze an 'ISet' after a specified callback/handler is done running.  This
-- differs from `withCallbacksThenFreeze` by not taking an additional action to run in
-- the context of the handlers.
-- 
--    (@'freezeSetAfter' 's' 'f' == 'withCallbacksThenFreeze' 's' 'f' 'return ()' @)
freezeSetAfter :: ISet s a -> (a -> QPar s ()) -> QPar s ()
freezeSetAfter s f = withCallbacksThenFreeze s f (return ())

-- | Register a per-element callback, then run an action in this context, and freeze
-- when all (recursive) invocations of the callback are complete.  Returns the final
-- value of the provided action.
withCallbacksThenFreeze :: Eq b => ISet s a -> (a -> QPar s ()) -> QPar s b -> QPar s b
withCallbacksThenFreeze (ISet (WrapLVar lv)) callback action =
    do
       hp  <- newPool 
       res <- IV.new -- TODO, specialize to skip this when the init action returns ()
       WrapPar$ 
         freezeLVAfter lv (initCB hp res) deltCB
       -- We additionally have to quiesce here because we fork the inital set of
       -- callbacks on their own threads:
       quiesce hp
       IV.get res
  where
    deltCB x = return$ Just$ unWrapPar$ callback x
    initCB hp resIV ref = do
      -- The implementation guarantees that all elements will be caught either here,
      -- or by the delta-callback:
      set <- readIORef ref -- Snapshot
      return $ Just $ unWrapPar $ do
        F.foldlM (\() v -> forkHP (Just hp)$ callback v) () set -- Non-allocating traversal.
        res <- action -- Any additional puts here trigger the callback.
        IV.put_ resIV res

-- | Get the exact contents of the set.  As with any
-- quasi-deterministic operation, using `freezeSet` may cause your
-- program to exhibit a limited form of nondeterminism: it will never
-- return the wrong answer, but it may include synchronization bugs
-- that can (nondeterministically) cause exceptions.
--
-- This "Data.Set"-based implementation has the special property that
-- you can retrieve the full set without any `IO`, and without
-- nondeterminism leaking.  (This is because the internal order is
-- fixed for the tree-based representation of sets that "Data.Set"
-- uses.)
freezeSet :: ISet s a -> QPar s (S.Set a)
freezeSet (ISet (WrapLVar lv)) = WrapPar $ 
   do freezeLV lv
      getLV lv globalThresh deltaThresh
  where
    globalThresh _  False = return Nothing
    globalThresh ref True = fmap Just $ readIORef ref
    deltaThresh _ = return Nothing

-- | /O(1)/: Convert from an `ISet` to a plain `Data.Set`.
--   This is only permitted when the `ISet` has already been frozen.
--   This is useful for processing the result of `Control.LVish.DeepFrz.runParThenFreeze`. 
fromISet :: ISet Frzn a -> S.Set a 
-- Alternate names? -- toPure? toSet? fromFrzn??
fromISet (ISet lv) = unsafeDupablePerformIO (readIORef (state lv))


--------------------------------------------------------------------------------

-- | Add an (asynchronous) callback that listens for all new elements added to
-- the set, optionally enrolled in a handler pool.
forEachHP :: Maybe HandlerPool           -- ^ pool to enroll in, if any
          -> ISet s a                    -- ^ Set to listen to
          -> (a -> Par d s ())           -- ^ callback
          -> Par d s ()
forEachHP hp (ISet (WrapLVar lv)) callb = WrapPar $ do
    L.addHandler hp lv globalCB (\x -> return$ Just$ unWrapPar$ callb x)
    return ()
  where
    globalCB ref = do
      set <- readIORef ref -- Snapshot
      return $ Just $ unWrapPar $ 
        F.foldlM (\() v -> forkHP hp $ callb v) () set -- Non-allocating traversal.

-- | Add an (asynchronous) callback that listens for all new elements added to
-- the set.
forEach :: ISet s a -> (a -> Par d s ()) -> Par d s ()
forEach = forEachHP Nothing

-- | Put a single element in the set.  (WHNF) Strict in the element being put in the
-- set.     
insert :: Ord a => a -> ISet s a -> Par d s ()
insert !elm (ISet (WrapLVar lv)) = WrapPar$ putLV lv putter
  where putter ref  = atomicModifyIORef ref update
        update set =
          let set' = S.insert elm set in
          -- Here we do a constant time check to see if we actually changed anything:
          -- For idempotency it is important that we return Nothing if not.
          if S.size set' > S.size set
          then (set',Just elm)
          else (set, Nothing)


-- | Wait for the set to contain a specified element.
waitElem :: Ord a => a -> ISet s a -> Par d s ()
waitElem !elm (ISet (WrapLVar lv)) = WrapPar $
    getLV lv globalThresh deltaThresh
  where
    globalThresh ref _frzn = do
      set <- readIORef ref
      case S.member elm set of
        True  -> return (Just ())
        False -> return (Nothing)
    deltaThresh e2 | e2 == elm = return $ Just ()
                   | otherwise  = return Nothing 


-- | Wait on the /size/ of the set, not its contents.
waitSize :: Int -> ISet s a -> Par d s ()
waitSize !sz (ISet lv) = WrapPar$
    getLV (unWrapLVar lv) globalThresh deltaThresh
  where
    globalThresh ref _frzn = do
      set <- readIORef ref
      case S.size set >= sz of
        True  -> return (Just ())
        False -> return (Nothing)
    -- Here's an example of a situation where we CANNOT TELL if a delta puts it over
    -- the threshold.a
    deltaThresh _ = globalThresh (state lv) False

--------------------------------------------------------------------------------
-- Higher level routines that could be defined using the above interface.
--------------------------------------------------------------------------------

-- | Return a fresh set which will contain strictly more elements than the input set.
-- That is, things put in the former go in the latter, but not vice versa.
copy :: Ord a => ISet s a -> Par d s (ISet s a)
copy = traverseSet return

-- | Establish a monotonic map between the input and output sets.
traverseSet :: Ord b => (a -> Par d s b) -> ISet s a -> Par d s (ISet s b)
traverseSet f s = traverseSetHP Nothing f s

-- | An imperative-style, in-place version of 'traverseSet' that takes the output set
-- as an argument.
traverseSet_ :: Ord b => (a -> Par d s b) -> ISet s a -> ISet s b -> Par d s ()
traverseSet_ f s o = void $ traverseSetHP_ Nothing f s o

-- | Return a new set which will (ultimately) contain everything in either input set.
union :: Ord a => ISet s a -> ISet s a -> Par d s (ISet s a)
union = unionHP Nothing

-- | Build a new set which will contain the intersection of the two input sets.
intersection :: Ord a => ISet s a -> ISet s a -> Par d s (ISet s a)
intersection = intersectionHP Nothing

-- | Take the cartesian product of two sets.
cartesianProd :: (Ord a, Ord b) => ISet s a -> ISet s b -> Par d s (ISet s (a,b))
cartesianProd s1 s2 = cartesianProdHP Nothing s1 s2 
  
-- | Take the cartesian product of several sets.
cartesianProds :: Ord a => [ISet s a] -> Par d s (ISet s [a])
cartesianProds ls = cartesianProdsHP Nothing ls

--------------------------------------------------------------------------------
-- Alternate versions of functions that EXPOSE the HandlerPools
--------------------------------------------------------------------------------

-- | Variant of `traverseSet` that optionally ties the handlers to a pool.
traverseSetHP :: Ord b => Maybe HandlerPool -> (a -> Par d s b) -> ISet s a ->
                 Par d s (ISet s b)
traverseSetHP mh fn set = do
  os <- newEmptySet
  traverseSetHP_ mh fn set os  
  return os

-- | Variant of `traverseSet_` that optionally ties the handlers to a pool.
traverseSetHP_ :: Ord b => Maybe HandlerPool -> (a -> Par d s b) -> ISet s a -> ISet s b ->
                  Par d s ()
traverseSetHP_ mh fn set os = do
  forEachHP mh set $ \ x -> do 
    x' <- fn x
    insert x' os

-- | Variant of `union` that optionally ties the handlers in the resulting set to the same
-- handler pool as those in the two input sets.
unionHP :: Ord a => Maybe HandlerPool -> ISet s a -> ISet s a -> Par d s (ISet s a)
unionHP mh s1 s2 = do
  os <- newEmptySet
  forEachHP mh s1 (`insert` os)
  forEachHP mh s2 (`insert` os)
  return os

-- | Variant of `intersection` that optionally ties the handlers in the resulting set to the same
-- handler pool as those in the two input sets.
intersectionHP :: Ord a => Maybe HandlerPool -> ISet s a -> ISet s a -> Par d s (ISet s a)
-- Can we do intersection with only the public interface?  It should be monotonic.
-- Well, for now we cheat and use liftIO:
intersectionHP mh s1 s2 = do
  os <- newEmptySet
  forEachHP mh s1 (fn os s2)
  forEachHP mh s2 (fn os s1)
  return os
 where  
  fn outSet (ISet lv) elm = do
    -- At this point 'elm' has ALREADY been added to "us", we check "them":    
    peek <- LI.liftIO$ readIORef (state lv)
    if S.member elm peek 
      then insert elm outSet
      else return ()

-- | Variant of 'cartesianProd' that optionally ties the handlers to a pool.
cartesianProdHP :: (Ord a, Ord b) => Maybe HandlerPool -> ISet s a -> ISet s b ->
                   Par d s (ISet s (a,b))
cartesianProdHP mh s1 s2 = do
  -- This is implemented much like intersection:
  os <- newEmptySet
  forEachHP mh s1 (fn os s2 (\ x y -> (x,y)))
  forEachHP mh s2 (fn os s1 (\ x y -> (y,x)))
  return os
 where
  -- This is expensive, but we've got to do it from both sides to counteract races:
  fn outSet (ISet lv) cmbn elm1 = do
    peek <- LI.liftIO$ readIORef (state lv)
    F.foldlM (\() elm2 -> insert (cmbn elm1 elm2) outSet) () peek


-- | Variant of 'cartesianProds' that optionally ties the handlers to a pool.
cartesianProdsHP :: Ord a => Maybe HandlerPool -> [ISet s a] ->
                    Par d s (ISet s [a])
cartesianProdsHP _ [] = newEmptySet
cartesianProdsHP mh ls = do
#if 1
  -- Case 1: recursive definition in terms of pairwise products:
  -- It would be best to create a balanced tree of these, I believe:
  let loop [lst]     = traverseSetHP mh (\x -> return [x]) lst -- Inefficient!
      loop (nxt:rst) = do
        partial <- loop rst
        p1      <- cartesianProdHP mh nxt partial
        traverseSetHP mh (\ (x,tl) -> return (x:tl)) p1 -- Inefficient!!
  loop ls
#else
  os <- newEmptySet
  let loop done [] acc = acc
      loop done (nxt:rest) acc =
        addHandler hp nxt (fn os done rest)
        
--  forM_ ls $ \ inSet -> do 
--    addHandler hp s1 (fn os s2 (\ x y -> (x,y)))

  return os
 where
  fn outSet left right newElm = do
    peeksL <- liftIO$ mapM (readIORef . state . unISet) left
    peeksR <- liftIO$ mapM (readIORef . state . unISet) right

--    F.foldlM (\() elm2 -> insert (cmbn elm1 elm2) outSet) () peek
    return undefined
#endif

