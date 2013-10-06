{-# LANGUAGE TypeFamilies, ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

import qualified Control.Monad.Par as MP 
import qualified Data.LVar.IVar as LI
import qualified Control.LVish as LVish

import qualified Control.Monad.State as SM

import Control.DeepSeq
import GHC.Prim(Constraint)

-- | @ParFuture@ captures the class of Par monads which support
--   futures.  This level of functionality subsumes @par@/@pseq@ and is
--   similar to the "Control.Parallel.Strategies.Eval" monad.
--
--   A minimal implementation consists of `spawn_` and `get`.
--   However, for monads that are also a member of `ParIVar` it is
--   typical to simply define `spawn` in terms of `fork`, `new`, and `put`.
-- 
class Monad m => ParFuture future m | m -> future where

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
  spawn  :: (NFData a, FutContents m a) => m a -> m (future a)
  
  -- | Like 'spawn', but the result is only head-strict, not fully-strict.
  spawn_ :: FutContents m a => m a -> m (future a)

  -- | Wait for the result of a future, and then return it.
  get    :: future a -> m a

  -- | Spawn a pure (rather than monadic) computation.  Fully-strict.
  --
  -- >  spawnP = spawn . return
  spawnP :: (NFData a, FutContents m a) =>   a -> m (future a)

  -- Default implementations:
  spawn  p = spawn_ (do x <- p; deepseq x (return x))
  spawnP a = spawn (return a)


-- We achieve backwards compatibility simply by putting in a null constraint:

instance ParFuture MP.IVar MP.Par where
  type FutContents MP.Par a = () -- No constraints!
  get    = MP.get
  spawn  = MP.spawn
  spawn_ = MP.spawn_
  spawnP = MP.spawnP

stcomp :: SM.MonadState String m => m String
stcomp = do SM.put "hi"; SM.get

instance ParFuture (LI.IVar s) (LVish.Par d s) where
  type FutContents (LVish.Par d s) a = (Eq a)
  spawn_ m = LI.spawn_ m
  get iv = LI.get iv

--------------------------------------------------------------------------------

-- We can't infer the type for this!!
-- The same problem as other overloaded families of monads (e.g. MonadState)  
par :: (ParFuture fut p, FutContents p String) => p String
par = do x <- spawn $ return "hello"
         get x
test2 :: String
test2 = MP.runPar par

test3 :: String
test3 = LVish.runPar par

-- par2 = do x <- spawn $ return "hello"; get x
par2 :: (ParFuture fut p, FutContents p Foo) => p Foo
par2 = do x <- spawn_$ return Bar; get x

-- If we skip the Eq here we get an error:
data Foo = Bar | Baz deriving (Show, Eq)

test4 :: Foo
test4 = LVish.runPar par2

main = print (test2, test3, test4)
