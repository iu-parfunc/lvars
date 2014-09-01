{-# LANGUAGE DataKinds, KindSignatures, GADTs, TypeOperators, CPP,
    GeneralizedNewtypeDeriving, FlexibleInstances, TypeFamilies, RankNTypes,
    ConstraintKinds, FlexibleContexts, UndecidableInstances, ScopedTypeVariables #-}

-- APPROACH: Expose type level products explicitly in all methods.

module Ver2_RawProds where

import Control.Monad
import Control.Applicative 
import Control.Monad.Trans.Class
import Common
import GHC.Exts

--------------------------------------------------------------------------------

-- type ReadOnly e  = (GetP e ~ NP, GetB e ~ NB , GetF e ~ NF , GetI e ~ NI)

type family ReadOnly (e :: EffectsSig) :: Constraint
-- type instance (ReadOnly (Ef p g f b i)) = (p ~ NP, b ~ NB , f ~ NF , i ~ NI)
type instance (ReadOnly (Ef NP g NF NB NI)) = ()
-- (GetP e ~ NP, GetB e ~ NB , GetF e ~ NF , GetI e ~ NI)

-- type HasPut e    = (GetP e ~ P)

type family HasPut (e :: EffectsSig) :: Constraint
type instance (HasPut (Ef p g f b i)) = (p ~ P)


type HasGet e    = (GetG e ~ G)
type HasFreeze e = (GetF e ~ F)
type HasIO  e    = (GetI e ~ I)

type NoFreeze e = (NF ~ GetF e)

type ReadOnlyM m = (ReadOnly (GetEffects m))

type HasIOM m = HasIO (GetEffects m)

----------------------------------------


--------------------------------------------------------------------------------
-- Core ops:

-- put :: IVar s a -> a -> Par (Ef P g f b) s ()
put :: HasPut e => IVar s a -> a -> Par e s ()
put = undefined

get :: IVar s a -> Par (Ef p G f b i) s a
get = undefined

freeze :: HasFreeze e => IVar s a -> Par e s (Maybe a)
freeze = undefined

new :: Par e s (IVar s a)
new = undefined

--------------------------------------------------------------------------------
-- Cancelation
--------------------------------------------------------------------------------

-- runCancelT :: CancelT (Par (Ef NP g NF NB) s) a -> Par (Ef p g f b) s a
runCancelT :: LVarMonad m => CancelT m a -> m a
runCancelT = undefined

-- Cancelable future
data CFut a = CFut

forkCancelable :: (LVarMonad m, ReadOnlyM m ) =>
                   CancelT m a -> CancelT m (CFut a)
forkCancelable = undefined

-- Non-deterministic version
forkCancelableND :: (LVarMonad m, HasIOM m) =>
                     CancelT m a -> CancelT m (CFut a)
forkCancelableND = undefined

cancel :: LVarMonad m => (CFut a) -> CancelT m ()
cancel = undefined

wait :: LVarMonad m => CFut a -> CancelT m a 
wait = undefined

--------------------------------------------------------------------------------
-- Memoization:
--------------------------------------------------------------------------------
data Memo (e :: EffectsSig) s a b = Memo

getMemo :: (Ord k, Eq v, e ~ Ef P G f b i) =>
           Memo e s k v  -> a -> Par e s v
getMemo = undefined

makeMemo :: (Ord a, Eq b) =>
            (a -> Par e s b) ->
            Par e s (Memo e s a b)
makeMemo = undefined

getMemoRO :: (Ord a, Eq b) =>
  Memo (Ef NP g NF NB NI) s a b -> a ->
  Par  (Ef p  G f  b_ i) s b 
getMemoRO = undefined

--------------------------------------------------------------------------------
-- Async and and typechecking

-- Stopped working in GHC 7.8:

-- asyncAnd introduces a get effect:
-- asyncAnd' :: forall p g f b i m1 m2 . 
--              (GetEffects m1 ~ Ef NP g NF NB NI,
--               m2 ~ SetEffects (Ef p G f b i) m1,
--               LVarMonad m1, LVarMonad m2) =>
--   CancelT m1 Bool -> CancelT m1 Bool -> CancelT m2 Bool
-- asyncAnd' = undefined  


-- type ROMemo g s a b = Memo (Ef NP g NF NB NI) s a b
-- data Type = Pair Type Type | TInt
--   deriving (Show,Read,Eq,Ord)

-- subtype :: ROMemo g s (Type,Type) Bool -> Type -> Type -> 
--              CancelT (Par (Ef p G f b i) s) Bool
-- subtype mem s t = 
--   case (s,t) of
--     (Pair s1 s2, Pair t1 t2) -> 
--        -- getMemoRO mem (s1, t1) -- type checks
--        asyncAnd' (lift$ getMemoRO mem (s1, t1))
--                  (lift$ getMemoRO mem (s2, t2))

--------------------------------------------------------------------------------
{-
-- runPar :: (forall s . Par (Ef p g NF b) s a) -> a
runPar :: NoFreeze e => (forall s . Par e s a) -> a
runPar = undefined


runDeadlockT :: LVarMonad m => 
                (forall s . s ~ GetSession m => DeadlockT m a)
              -> m (Maybe a)
runDeadlockT = undefined

-- We can perform put or freeze operation to the world outside a `runTillDeadlock`,
-- but we cannot do blocking reads.
runTillDeadlockTWW :: forall mOut mNoGet m2 p g f b res . 
   (mNoGet ~ SetMG NG mOut,
   LVarMonad mOut, LVarMonad mNoGet) => 
  (forall s2 mFresh . (LVarMonad mFresh, mFresh ~ (SetSession s2 mOut)) =>
         (forall a . mNoGet a -> DeadlockT mFresh a)
         -> DeadlockT mFresh res) ->
  mOut (Maybe res)

runTillDeadlockTWW = undefined

data IMap k s v = IMap

newEmptyMap :: Par e s (IMap k s v)
newEmptyMap = undefined

insert :: (Ord k , Eq v) =>
          k -> v -> IMap k s v -> Par (Ef P g f b i) s () 
insert = undefined

getKey :: Ord k => k -> IMap k s v -> Par (Ef p G f b i) s v
getKey = undefined

t :: Par (Ef P g f b i) s1 (Maybe Int)
t = do 
  mp <- newEmptyMap
  insert "hmm" "ok" mp -- Without this I get a perverse and strange error:
  runTillDeadlockTWW $ \ esc -> do
        esc $ do insert "hi" "there" mp
                 -- getKey "hi" mp -- Example error.  Good localization.
                 return ()
        return 3


test :: IVar s String -> Par (Ef P G f b i) s String
test iv = do put iv "hi"
             get iv

test2 :: (HasPut e, HasFreeze e) => Par e s (Maybe String) 
test2 = do iv <- new
           put iv "hi"
           freeze iv

test3 :: Par (Ef P G f b i) s String
test3 = do iv <- new
           put iv "hi"
           get iv

_ = runPar test3

-- _ = runPar test2
-- Couldn't match type 'F with 'NF

------------------------------------------------------------
-}

--------------------------------------------------------------------------------

liftReadOnly :: Par (Ef NP g NF NB NI) s a -> Par (Ef p g f b i) s a
liftReadOnly = undefined

-- A trivial step
liftReadOnly2 :: CancelT (Par (Ef NP g NF NB NI) s) a 
              -> CancelT (Par (Ef p g f b i) s)     a
liftReadOnly2 = undefined

-- Here's where we run into trouble:
-- This would be a rigid type variable problem:
-- liftReadOnly3 :: forall m1 m2 p g f b i a . 
--                  ( GetEffects m1 ~ (Ef NP g NF NB NI)
--                  , m2 ~ SetEffects (Ef p g f b i) m1) 
--               => CancelT m1 a 
--               -> CancelT m2 a
-- liftReadOnly3 = undefined


type ReadOnlyOf m = (SetEffects (Ef NP (GetG (GetEffects m)) NF NB NI) m)

-- This, on the other hand, works fine:
liftReadOnly4 :: forall m a . (LVarMonad m) 
              => (ReadOnlyOf m) a 
              -> m a
liftReadOnly4 = undefined

--------------------------------------------------------------------------------

c1 :: CancelT (Par (Ef NP G NF NB NI) s) Int
c1 = undefined

c2 :: CancelT (Par (Ef P G F B I) s) Int
c2 = liftReadOnly4 c1

c3 :: CancelT (Par (Ef p G f b i) s) Int
c3 = liftReadOnly4 c1

-- Error, not read only:
-- c4 :: CancelT (Par (Ef p G f b i) s) Int
-- c4 = liftReadOnly4 c2

main :: IO ()
main = putStrLn "hi"


