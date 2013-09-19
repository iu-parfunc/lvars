-- | A Scalable Non-Zero Indicator
--
-- A SNZI is a kind of concurrent counter which can be incremented, decremented,
-- and queried for equality with 0.  The interface is a bit more complex,
-- though: it is exposed as N values (where N = the number of CPUs), each
-- providing an @arrive@ and @depart@ operation, together with a single polling
-- action querying the value of the counter.  The client MUST NOT invoke
-- @depart@ more times than @arrive@ on any single value.
--
-- The implementation is based on http://dl.acm.org/citation.cfm?id=1281106, but
-- significantly simplified by allowing a call to @arrive@ to block indefinitely
-- until other such calls complete.  (Thus the algorithm is no longer
-- non-blocking in theory; its liveness depends on assuming that the OS-level
-- thread scheduler is fair.)
--
-- The basic design is to have a *tree* of counters; each child node in the tree
-- is allowed to invoke @arrive@/@depart@ on its parents.  There are two invariants:
-- 
--   * The number of @depart@s (decrements) must never outnumber the @arrive@s
--   (increments) at any point in the tree.  This invariant is partially
--   dependent on the client, which must ensure it for the exposed leaf
--   counters.
--
--   * The number of @arrive@s a child has invoked on a parent can outnumber the
--   @depart@s iff the total number of arrives at the child outnumbers the departs
--   at the child.
--
-- The idea is that child nodes act as "filters": they only need to invoke
-- @arrive@/@depart@ on their parents when their own value changes from 0 to 1 or 1
-- to 0 (i.e., when they change to/from having a surplus).
--
-- To maintain the above invariants, however, child nodes use a special
-- representation: if n >= 0, it represents the counter, but if n = -1 the child
-- is "locked".  The locked value is needed to handle races between @arrive@s
-- when the node is currently at 0.  The thread that wins the race will move the
-- counter from 0 to -1, thereby effectively "locking" it.  Subsequently, it
-- will invoke @arrive@ on the parent, and then finally "unlock" the counter by
-- setting it to the value 1.  See the paper for details on why a protocol like
-- this is needed (the paper uses a more complex, lock-free protocol).  Such a
-- protocol is *not* needed for @depart@, however.

module Data.Concurrent.SNZI
where
  
import System.IO.Unsafe
import Control.Reagent  
import Control.Monad
import GHC.Conc
import Data.IORef
import Data.Atomics
import Data.Concurrent.AlignedIORef
  
-- | An entry point for a shared SNZI value
data SNZI = 
    Child (AlignedIORef Int) SNZI
  | Root  (AlignedIORef Int)

-- | Signal the presence of a thread at a SNZI
arrive :: SNZI -> IO ()    
arrive (Root cnt) = react $ atomicUpdate_ (ref cnt) (+1)
arrive (Child cnt parent) = 
  let upd 0    = Just (-1, True)
      upd (-1) = Nothing
      upd n    = Just (n+1, False)
  in do
    tellParent <- react $ atomicUpdate (ref cnt) upd
    when tellParent $ do
      arrive parent
      writeBarrier
      writeIORef (ref cnt) 1
  
data TellParent = Yes | No | Err
    
-- | Signal the departure of a thread at a SNZI.  IMPORTANT: depart MUST NOT be
-- called more times than arrive for a given SNZI value.
depart :: SNZI -> IO ()  
depart (Root cnt) = react $ atomicUpdate_ (ref cnt) (\x -> x-1)
depart (Child cnt parent) = 
  let upd 0    = Just (0, Err)
      upd (-1) = Nothing
      upd 1    = Just (0,   Yes)
      upd n    = Just (n-1, No)
  in do
    tellParent <- react $ atomicUpdate (ref cnt) upd
    case tellParent of
      No  -> return ()
      Yes -> depart parent
      Err -> do putStrLn "BUG: departs outnumber arrives"
                error "BUG: departs outnumber arrives"
    
-- Helper function to generate a tree of SNZI values.
makeTree :: Int -> [SNZI] -> [SNZI] -> IO [SNZI]
makeTree n parents children = 
  if n >= numCapabilities then return children 
  else case parents of 
    [] -> makeTree 0 children []
    (parent:parents') -> do
      c1 <- newAlignedIORef 0
      c2 <- newAlignedIORef 0
      makeTree (n+2) parents' $ Child c1 parent : Child c2 parent : children
  
-- | Create a shared SNZI values with numCapabilities number of entry points,
-- together with a polling action that returns "true" when no threads are
-- present.
newSNZI :: IO ([SNZI], IO Bool)
newSNZI = do
  rootRef <- newAlignedIORef 0
  leaves  <- makeTree 1 [] [Root rootRef]
  return (leaves, readIORef (ref rootRef) >>= return . (== 0))