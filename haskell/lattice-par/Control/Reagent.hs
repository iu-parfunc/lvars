{-# LANGUAGE ExplicitForAll, Rank2Types #-} 

-- | An implementation of Reagents (http://www.mpi-sws.org/~turon/reagents.pdf)
-- NOTE: currently this is just a very tiny core of the Reagent design.  Needs
-- lots of work.

module Control.Reagent
where
  
import Data.IORef  
import Data.Atomics

type Reagent a = forall b. (a -> IO b) -> IO b -> IO b

-- | Execute a Reagent.
{-# INLINE react #-}
react :: Reagent a -> IO a
react r = try where 
  try      = r finish try
  finish x = return x
  
-- | Like atomicModifyIORef, but uses CAS and permits the update action to force
-- a retry by returning Nothing  
  
{-# INLINE atomicUpdate #-}
atomicUpdate :: IORef a -> (a -> Maybe (a, b)) -> Reagent b  
atomicUpdate r f succ fail = do
  curTicket <- readForCAS r
  let cur = peekTicket curTicket
  case f cur of
    Just (new, out) -> do
      (done, _) <- casIORef r curTicket new
      if done then succ out else fail
    Nothing -> fail
atomicUpdate_ :: IORef a -> (a -> a) -> Reagent ()
atomicUpdate_ r f = atomicUpdate r (\x -> Just (f x, ()))
    
postCommit :: Reagent a -> (a -> IO b) -> Reagent b
postCommit r f succ fail = r (\x -> f x >>= succ) fail

choice :: Reagent a -> Reagent a -> Reagent a
choice = error "TODO"