{-# LANGUAGE Trustworthy #-}

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
-- FlexibleInstances, UndecidableInstances

{-# LANGUAGE TypeFamilies, ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DefaultSignatures #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}

{-|
    This module establishes a class hierarchy that captures the
    interfaces of @Par@ monads.  There are two layers: simple futures
    ('ParFuture') and full @IVars@ ('ParIVar').  All @Par@ monads are
    expected to implement the former, some also implement the latter.

    For more documentation of the programming model, see

    * The "Control.Monad.Par" module in the @monad-par@ package.

    * The wiki\/tutorial (<http://www.haskell.org/haskellwiki/Par_Monad:_A_Parallelism_Tutorial>)

    * The original paper (<http://www.cs.indiana.edu/~rrnewton/papers/haskell2011_monad-par.pdf>)

    * Tutorial slides (<http://community.haskell.org/~simonmar/slides/CUFP.pdf>)

    * Other slides (<http://www.cs.ox.ac.uk/ralf.hinze/WG2.8/28/slides/simon.pdf>, <http://www.cs.indiana.edu/~rrnewton/talks/2011_HaskellSymposium_ParMonad.pdf>)

 -}
--

module Control.Par.Class
  (
  -- * The essence of Par monads: forking control flow
    ParMonad(fork)
  -- * Futures: basic parallelism with communication
  , ParFuture(..)
  -- * IVars: futures that anyone can fill
  , ParIVar(..)

  --  Monotonically growing finite maps
--  , ParIMap(..) -- Not ready yet.

   -- * Data structures that can be consumed in parallel
  , ParFoldable(..)
  , Generator(..)    
    
   -- * (Internal) Abstracting LVar Schedulers.
  , LVarSched(..), LVarSchedQ(..)
  , ParQuasi (..), ParSealed(..)
  , Proxy(Proxy)
                   
    -- RRN: Not releasing this interface until there is a nice implementation of it:
    --  Channels (Streams)
    --  , ParChan(..)

  -- * Simple tracking of WHICH Par monads permit only threadsafe effects
  , ParThreadSafe()
    
  , NFData() -- This is reexported.
  )
where

import Control.DeepSeq
import Control.Par.Class.Unsafe
import Data.Splittable.Class as Sp 
import GHC.Prim (Constraint)

import qualified Data.Foldable as F

--------------------------------------------------------------------------------

-- | @ParFuture@ captures the class of Par monads which support
--   futures.  This level of functionality subsumes @par@/@pseq@ and is
--   similar to the "Control.Parallel.Strategies.Eval" monad.
--
--   A minimal implementation consists of `spawn_` and `get`.
--   However, for monads that are also a member of `ParIVar` it is
--   typical to simply define `spawn` in terms of `fork`, `new`, and `put`.
--
class ParMonad m => ParFuture m where
-- class (Monad m, Functor m) => ParFuture m where
  -- | The type of a future that goes along with the particular `Par`
  -- monad the user chooses.
  type Future m :: * -> *

  -- | Different implementations may place different constraints on
  -- what is allowable inside a Future.  For example, some
  -- implementations require an Eq Constraint.
  type FutContents m a :: Constraint

  -- | Create a potentially-parallel computation, and return a /future/
  -- (or /promise/) that can be used to query the result of the forked
  -- computataion.
  --
  -- >  spawn p = do
  -- >    r <- new
  -- >    fork (p >>= put r)
  -- >    return r
  --
  spawn  :: (NFData a, FutContents m a) => m a -> m (Future m a)

-- | Like 'spawn', but the result is only head-strict, not fully-strict.
  spawn_ :: FutContents m a => m a -> m (Future m a)
              
  -- | Wait for the result of a future, and then return it.
  get    :: Future m a -> m a

  -- | Spawn a pure (rather than monadic) computation.  Fully-strict.
  --
  -- >  spawnP = spawn . return
  spawnP :: (NFData a, FutContents m a) =>   a -> m (Future m a)

  -- Default implementations:
  spawn  p = spawn_ (do x <- p; deepseq x (return x))
  spawnP a = spawn (return a)

  default spawn_ :: (ParIVar m, FutContents m a) => m a -> m (Future m a)
  spawn_ p = do r <- new;  fork (p >>= put_ r);  return r

--------------------------------------------------------------------------------

-- | A simple type alias.  A hint.
type IVar m a = Future m a

-- | @ParIVar@ builds on futures by adding full /anyone-writes, anyone-reads/ IVars.
--   These are more expressive but may not be supported by all distributed schedulers.
--
-- A minimal implementation consists of `fork`, `put_`, and `new`.
class ParFuture m  => ParIVar m  where
  
  -- | creates a new @IVar@
--  new  :: m (IVar m a)
  new  :: forall frsh . FutContents m frsh => m (Future m frsh) 

  -- | put a value into a @IVar@.  Multiple 'put's to the same @IVar@
  -- are not allowed, and result in a runtime error.
  --
  -- 'put' fully evaluates its argument, which therefore must be an
  -- instance of 'NFData'.  The idea is that this forces the work to
  -- happen when we expect it, rather than being passed to the consumer
  -- of the @IVar@ and performed later, which often results in less
  -- parallelism than expected.
  --
  -- Sometimes partial strictness is more appropriate: see 'put_'.
  --
  put  :: forall a . (FutContents m a, NFData a) =>
          IVar m a -> a -> m ()
  put v a = deepseq a (put_ v a)
  
  -- | like 'put', but only head-strict rather than fully-strict.  
  put_ :: forall a . (FutContents m a) =>
          IVar m a -> a -> m ()

  -- Extra API routines that have default implementations:

  -- | creates a new @IVar@ that contains a value
  newFull :: (FutContents m a, NFData a) => a -> m (IVar m a)
  newFull a = deepseq a (newFull_ a)

  -- | creates a new @IVar@ that contains a value (head-strict only)
  newFull_ :: FutContents m a => a -> m (IVar m a)
  newFull_ a = do v <- new
                  -- This is usually inefficient!
        	  put_ v a
        	  return v

--------------------------------------------------------------------------------

-- | Carry a phantom type argument.
data Proxy a = Proxy

-- TODO: Move to Control.LVish.Class:

-- | Par monads which can be switched into a "quasi-deterministic" variant.
--   (See Kuper et al. POPL 2014).
class (Monad m, Functor m, Monad qm, Functor qm) => ParQuasi m qm | m -> qm where
   -- | Lift a deterministic computation to a quasi-deterministic one.  
   toQPar :: m a -> qm a 
  
-- | All proper @Par@ monads should have an @s@ parameter that seals them so that
-- live Futures, IVars, etc cannot escape from a Par computation.  Membership in this
-- class provides a uniform way to extract these @s@ parameters where needed.
class ParSealed (m :: * -> *) where
   -- | Extract the @s@ parameter from a Par monad type.
   type GetSession m :: *

-- | Abstract over LVar-capable /schedulers/.  This is not for end-user programming.
class (Monad m, ParSealed m) => LVarSched m  where
   -- | The type of raw LVars, shared among all specific data structures (Map, Set, etc).
   type LVar m :: * -> * -> *

   -- | Create an LVar, including the extra state that required for scheduling and
   -- synchronization.
   newLV :: IO a -> m (LVar m a d)

   -- | Update an LVar.  The change itself happens as an IO action, but any state
   -- changes must be linearizable and must respect the lattice semantics for the LVar.
   putLV :: LVar m a d           -- ^ the LVar
         -> (a -> IO (Maybe d))  -- ^ how to do the put, and whether the LVar's
                                 --   value changed
         -> m ()

   -- | Do a threshold read on an LVar.  This requires both a "global" and "delta"
   -- handler, and if the underlying Par monad assumes idempotency, both could even
   -- execute for the same value.
   getLV :: (LVar m a d)                -- ^ the LVar 
         -> (a -> Bool -> IO (Maybe b)) -- ^ already past threshold?
                                        -- The @Bool@ indicates whether the LVar is FROZEN.
         -> (d ->         IO (Maybe b)) -- ^ does @d@ pass the threshold?
         -> m b

   -- | Extract a handle on the raw, mutable state within an LVar.
   stateLV :: (LVar m a d) -> (Proxy (m ()), a)

   -- addHandler

   -- | Fork a child thread.   
   forkLV :: m () -> m ()

   -- | Put ourselves at the bottom of the work-pile for the current thread, allowing
   -- others a chance to run.
   yield :: m ()
   yield = return ()

   -- | Usually a no-op.  This checks in with the scheduler, so that it might perform
   -- any tasks which need to be performed periodically.
   schedCheck :: m ()
   schedCheck = return ()

   -- | Drop the current continuation and return to the scheduler.
   returnToSched :: m a

   -- TODO: should we expose a MonadCont instance?

-- | An LVar scheduler with the quasi-determinism capability.  This interface is
--   complicated by the fact that there are two monads (deterministic and
--   quasideterministic).
class (Monad qm, LVarSched m, ParQuasi m qm) => LVarSchedQ m qm | m -> qm where
  
   -- | Freeze an LVar (introducing quasi-determinism).  This requires marking it at runtime.
   freezeLV :: LVar m a d -> (Proxy (m()), qm ())
   -- It is the implementor's responsibility to expose this as quasi-deterministic.

--------------------------------------------------------------------------------

--  | A commonly desired monotonic data structure is an insertion-only Map or Set.
--   This captures Par monads which are able to provide that capability.
class (Functor m, Monad m) => ParIMap m  where
  -- | The type of a future that goes along with the particular `Par`
  -- monad the user chooses.
  type IMap m k :: * -> *

  -- | Different implementations may place different constraints on
  -- what is allowable inside a Future.  For example, some
  -- implementations require an Eq Constraint.
  type IMapContents m k v :: Constraint

  -- | Wait on the /size/ of the map, not its contents.
  waitSize :: Int -> IMap m k v -> m ()

  -- | Create a fresh map with nothing in it.
  newEmptyMap :: m (IMap m k v)

  -- | Put a single entry into the map.  Strict (WHNF) in the key and value.
  -- 
  --   As with other container LVars, if a key is inserted multiple times, the values had
  --   better be equal @(==)@, or a multiple-put error is raised.
  insert :: (IMapContents m k v) => k -> v -> IMap m k v -> m ()

  -- | Wait for the map to contain a specified key, and return the associated value.
  getKey :: (IMapContents m k v) => k -> IMap m k v -> m v

  -- FINISHME -- other methods
  -- newMap newFromList
  -- modify, forEach, copy, union...

-- | Normal @IMap@ capabilities plus the additional capability of freezing @IMap@s.
class (ParQuasi m qm, ParIMap m) => ParIMapFrz m qm | m -> qm where
  -- | Get the exact contents of the map.  As with any
  -- quasi-deterministic operation, using `freezeMap` may cause your
  -- program to exhibit a limited form of nondeterminism: it will never
  -- return the wrong answer, but it may include synchronization bugs
  -- that can (nondeterministically) cause exceptions.
  --
  -- This "Data.Map"-based implementation has the special property that
  -- you can retrieve the full map without any `IO`, and without
  -- nondeterminism leaking.  (This is because the internal order is
  -- fixed for the tree-based representation of maps that "Data.Map"
  -- uses.)
  freezeMap :: IMap m k v -> qm (SomeFoldable (k,v))
  -- FIXME: We can't actually provide an instance of SomeFoldable (k,v) easily... [2013.10.30]

data SomeFoldable a = forall f2 . F.Foldable f2 => SomeFoldable (f2 a)


--------------------------------------------------------------------------------


-- | Collections that can generate a sequence of elements of the same element type.
-- 
--   Reason: We have a problem where some types (like Ranges) are splittable, but they are
--   not containers for arbitrary data.  Thus we introduce a more limited concept of
--   a data source that can generate only a particular kind of element (but cannot be
--   constructed or traversed).
--
--   It is trivial to provide an instance for any type that is already a `Functor`:
--   
-- > import Data.Foldable as F
-- > instance Foldable f => Generator (f a) where
-- >   type ElemOf (f a) = a
-- >   foldrM = F.foldrM 
--
--   However, we don't provide this blanket instance because it would conflict with
--   more tailored instances that may be desired for particular containers.  For
--   example, a "Data.Map" generator might include keys as well as values.
--
--   Finally, note that a much more general version of this class can be found in
--   "Data.Generator" from the reducers package.
class Generator c where
  type ElemOf c :: *
  -- | Fold all outputs from the generator, sequentially.
  --   The ordering is determined by the generator type, and is whatever
  --   the natural and inexpensive ordering for that type is.
  --        
  --   In general this should be used with commutative, associative functions.
  --   This fold is strict in the accumulator.       
  fold :: (acc -> ElemOf c -> acc) -> acc -> c -> acc

  -- | A monadic version of `fold`.
  foldM :: (Monad m) => (acc -> ElemOf c -> m acc) -> acc -> c -> m acc
  -- foldM :: (Monad m) => (ElemOf c -> acc -> m acc) -> acc -> c -> m acc  
  foldM fn zer = fold (\ !acc elm -> acc >>= (`fn2` elm)) (return zer)
     where fn2 !acc elm = fn acc elm

  -- | If the monad in question is a Par monad, then this version can sometimes be
  -- more efficient than `foldM`.
  foldMP :: (ParMonad m) => (acc -> ElemOf c -> m acc) -> acc -> c -> m acc
  foldMP f z c = foldM f z c
  -- This default implementation was going VERY slowly for me with the Trace
  -- scheduler (100X slower, [2013.12.07]).  It's weird though, because simply
  -- duplicating the foldM function for foldMP (Range instances) fixes the problem
  -- entirely.

  -- | Execute an action for each output of the generator.
  forM_ :: (Monad m) => c -> (ElemOf c -> m ()) -> m ()
  forM_ c fn = foldM (\ () x -> fn x) () c 

  -- | The same as `forM_` but for a Par monad.
  forMP_ :: (ParMonad m) => c -> (ElemOf c -> m ()) -> m ()
  forMP_ c fn= foldMP (\ () x -> fn x) () c 


-- instance F.Foldable f => Generator (f a) where
--   type ElemOf (f a) = a
--   {-# INLINE foldrM #-}
--   foldrM = F.foldrM 
   
--------------------------------------------------------------------------------

-- class Sp.Generator c e => ParFoldable c e | c -> e where

-- | Collection types which can be consumed in parallel.
class Generator c => ParFoldable c where  
  pmapFold :: forall m a t .
              (ParFuture m, FutContents m a)
              => (ElemOf c -> m a) -- ^ compute one result
              -> (a -> a -> m a)   -- ^ combine two results 
              -> a                 -- ^ initial accumulator value
              -> c                 -- ^ element generator to consume              
              -> m a

--------------------------------------------------------------------------------

-- class ParYieldable ??
  -- TODO: I think we should add yield officially:

  -- Allows other parallel computations to progress.  (should not be
  -- necessary in most cases).
  --  yield  :: m ()


--------------------------------------------------------------------------------

{-

-- | @ParChan@ provides communication via streams of values between
--   computations in a Par monad.  Channels in this case are split
--   into separate send and receive ports.
--
--   The critical thing to know about @Chan@s in @Par@ monads is that
--   while the @recv@ method destructively advances the position of
--   the consumer's \"cursor\" in the stream, this is only observable
--   in the /local/ @Par@ thread.  That is, at @fork@ points it is
--   necessary to give the child computation a separate set of stream
--   cursors so that it observes the same sequences as the parent.
class Monad m => ParChan snd rcv m | m -> snd, m -> rcv where
   -- | Create a new communication channel, with separate send and receive ports.
   newChan :: m (snd a, rcv a)
   -- | Receive a message on a channel in a synchronous, blocking manner.
   recv    :: rcv a -> m a
   -- | Send a message on a channel.  This may or may not block.
   send    :: snd a -> a -> m ()

-}

----------------------------------------------------------------------------------------------------

{-
-- t1 :: P.Par Int
-- If the ParIVar => ParFuture instance exists the following is sufficient:
t1 :: (ParFuture v m) => m Int
t1 = do
  x <- spawn (return 3)
  get x

t2 :: (ParIVar v m) => m Int
t2 = do
  x <- new
  put x "hi"
  return 3


-- TODO: SPECIALIZE generic routines for the default par monad (and possibly ParRNG)?

--  SPECIALISE parMap  :: (NFData b) => (a -> b)     -> [a] -> Par [b]
-- SPECIALISE parMapM :: (NFData b) => (a -> Par b) -> [a] -> Par [b]
-}
