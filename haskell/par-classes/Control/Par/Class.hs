{- LANGUAGE MultiParamTypeClasses, FunctionalDependencies, 
             FlexibleInstances, UndecidableInstances -}
{-# LANGUAGE TypeFamilies, ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- {-# LANGUAGE GADTs #-}

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
  -- * Futures: the most basic functionality
    ParFuture(..)
  -- * IVars
  , ParIVar(..)

  --  Monotonically growing finite maps
--  , ParIMap(..)
    
    -- RRN: Not releasing this interface until there is a nice implementation of it:
    --  Channels (Streams)
    --  , ParChan(..)

  , NFData() -- This is reexported.
  )
where

import Control.DeepSeq
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
-- class Monad m => ParFuture future m | m -> future where
class (Functor m, Monad m) => ParFuture m where
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

--------------------------------------------------------------------------------

-- | A simple type alias.  A hint.
type IVar m a = Future m a

-- | @ParIVar@ builds on futures by adding full /anyone-writes, anyone-reads/ IVars.
--   These are more expressive but may not be supported by all distributed schedulers.
--
-- A minimal implementation consists of `fork`, `put_`, and `new`.
class ParFuture m  => ParIVar m  where
  
  -- | Forks a computation to happen in parallel.  The forked
  -- computation may exchange values with other computations using
  -- @IVar@s.
  fork :: m () -> m ()

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

--------------------------------------------------------------------------------


--  | A commonly desired monotonic data structure an insertion-only Map or Set.
--   This captures Par monads which are able to provide that capability.
class (Functor m, Monad m) => ParIMap m  where
  -- | The type of a future that goes along with the particular `Par`
  -- monad the user chooses.
  type IMap m k :: * -> *

  -- | Different implementations may place different constraints on
  -- what is allowable inside a Future.  For example, some
  -- implementations require an Eq Constraint.
  type IMapContents m k v :: Constraint

  waitSize :: Int -> IMap m k v -> m ()

  newEmptyMap :: m (IMap m k v)

  insert :: (IMapContents m k v) => k -> v -> IMap m k v -> m ()

  getKey :: (IMapContents m k v) => k -> IMap m k v -> m v

  -- TODO: freezing?  How can we assert quasideterminism?

  type QPar m :: * -> *

--   freezeMap :: IMap m k v -> QPar m (SomeFoldable (k,v))

-- data SomeFoldable a = forall f2 . F.Foldable f2 => SomeFoldable (f2 a)

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
