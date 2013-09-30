
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.LVish.Classes where

import Control.Monad

import           Data.Hashable
import qualified Data.Set as Set

import qualified Control.LVish as L
import qualified Data.LVar.PureSet as S

-- | Abstract over the core scheduler functionality.  This is not safe for end users!
class Monad p => LParMonad p where
  type HandlerPool 
  type LVar a d
  -- state 
  -- yield, newPool, fork, forkInPool,
  testing :: Show HandlerPool => p ()

  addHandler :: HandlerPool                 -- ^ pool to enroll in 
             -> LVar a d                    -- ^ LVar to listen to
             -> (a -> IO (Maybe (p ())))  -- ^ initial callback
             -> (d -> IO (Maybe (p ())))  -- ^ subsequent callbacks: updates
             -> p ()

  yield :: p () 
  -- addHandler
  
-- class QParMonad q where
-- freeze

instance LParMonad L.Par where
  type HandlerPool = L.HandlerPool
  type LVar a d = L.LVar a d
  yield = L.yield

instance LParMonad L.QPar where
  type HandlerPool = L.HandlerPool
  type LVar a d = L.LVar a d
  yield = L.liftQ L.yield

#if 1
--------------------------------------------------------------------------------
-- This version allows only one "Set" for a given monad:
--------------------------------------------------------------------------------    
class LParMonad par => LParSet par where
  type ISet a  
  newEmptySet :: par (ISet a)
  newSet      :: Set.Set a -> par (ISet a)
  -- Default definition:
  --  newSet = do 
  putInSet :: (Hashable a, Ord a) => a -> ISet a -> par ()  

class LParSet par => QParSet par where
  freezeSet :: ISet a -> par (Set.Set a)
  
instance LParSet L.Par where
  type ISet a = S.ISet a
  newEmptySet = S.newEmptySet
  putInSet    = S.insert

instance LParSet L.QPar where
  type ISet a  = S.ISet a
  newEmptySet  = L.liftQ S.newEmptySet
  putInSet e s = L.liftQ$ S.insert e s

instance QParSet L.QPar where
  freezeSet    = S.freezeSet  
#else
--------------------------------------------------------------------------------
-- This versions allows a many-to-one relationship of data structures to monads:
--------------------------------------------------------------------------------
class LParMonad par => LParSet set par where  
  newEmptySet :: par (set a)
  newSet :: Set.Set a -> par (set a)
  -- Default definition:
  --  newSet = do 
  putInSet :: (Hashable a, Ord a) => a -> set a -> par ()

instance LParSet S.ISet L.Par where
  newEmptySet = S.newEmptySet
  putInSet = S.insert 
#endif


t0 = L.runParIO $ do
  s <- newEmptySet
  putInSet "hello" (s :: S.ISet String)
  L.logStrLn "Ran!"
  return ()

t1 = L.runQParIO $ do
  s <- newEmptySet
  putInSet "hello" (s :: S.ISet String)
  putInSet "there" s
  freezeSet s



{-
  newSet :: S.Set a -> Par (ISet a)
newFromList :: Ord a => [a] -> Par (ISet a)
freezeSetAfter :: ISet a -> (a -> QPar ()) -> QPar ()
withCallbacksThenFreeze :: Eq b => ISet a -> (a -> QPar ()) -> QPar b -> QPar b

addHandler :: HandlerPool                 -- ^ pool to enroll in 
forEach :: ISet a -> (a -> Par ()) -> Par HandlerPool

waitElem :: Ord a => a -> ISet a -> Par ()
waitSize :: Int -> ISet a -> Par ()
copy :: Ord a => ISet a -> Par (ISet a)
traverseSet :: Ord b => (a -> Par b) -> ISet a -> Par (ISet b)
traverseSet_ :: Ord b => (a -> Par b) -> ISet a -> ISet b -> Par ()
union :: Ord a => ISet a -> ISet a -> Par (ISet a)
intersection :: Ord a => ISet a -> ISet a -> Par (ISet a)
cartesianProd :: (Ord a, Ord b) => ISet a -> ISet b -> Par (ISet (a,b))
cartesianProds :: Ord a => [ISet a] -> Par (ISet [a])
forEachHP :: Maybe HandlerPool -> ISet a -> (a -> Par ()) -> Par HandlerPool
traverseSetHP :: Ord b => Maybe HandlerPool -> (a -> Par b) -> ISet a -> Par (HandlerPool, ISet b)
traverseSetHP_ :: Ord b => Maybe HandlerPool -> (a -> Par b) -> ISet a -> ISet b -> Par HandlerPool
cartesianProdHP :: (Ord a, Ord b) => Maybe HandlerPool -> ISet a -> ISet b -> Par (HandlerPool, ISet (a,b))
cartesianProdsHP :: Ord a => Maybe HandlerPool -> [ISet a] -> Par (HandlerPool, ISet [a])


class Monad m => ParFuture m where
  type Future m a
  
class ParFuture m => ParIVar m where
  type IVar m a
  

-}
     
