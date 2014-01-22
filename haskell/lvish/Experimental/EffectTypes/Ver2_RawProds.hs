{-# LANGUAGE DataKinds, KindSignatures, GADTs, TypeOperators, CPP,
    GeneralizedNewtypeDeriving, FlexibleInstances, TypeFamilies, RankNTypes,
    ConstraintKinds, FlexibleContexts  #-}

-- APPROACH: Expose type level products explicitly in all methods.

module Ver2_RawProds where

import Control.Monad
import Control.Applicative 
import Control.Monad.Trans.Class
import Common

--------------------------------------------------------------------------------
-- Core ops:

-- put :: IVar s a -> a -> Par (Ef P g f b) s ()
put :: HasPut e => IVar s a -> a -> Par e s ()
put = undefined

get :: IVar s a -> Par (Ef p G f b) s a
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

forkCancelable :: (ParMonad m, ReadOnly3 m ) =>
                   CancelT m () -> CancelT m TID
forkCancelable = undefined

cancel :: ParMonad m => TID -> CancelT m ()
cancel = undefined

--------------------------------------------------------------------------------
-- Memoization:
--------------------------------------------------------------------------------
data Memo (e :: EffectsSig) s a b = Memo

getMemo :: (Ord k, Eq v, e ~ Ef P G f b) =>
           Memo e s k v  -> a -> Par e s v
getMemo = undefined

makeMemo :: (Ord a, Eq b) =>
            (a -> Par e s b) ->
            Par e s (Memo e s a b)
makeMemo = undefined

getMemoRO :: (Ord a, Eq b) =>
  Memo (Ef NP g NF NB) s a b -> a ->
  Par  (Ef p  G f  b_) s b 
getMemoRO = undefined

--------------------------------------------------------------------------------
-- Async and and typechecking

asyncAnd' :: (GetEffects m1 ~ Ef NP g NF NB,
              m2 ~ SetEffects (Ef p G f b) m1,
              LVarMonad m1, LVarMonad m2) =>
  CancelT m1 Bool -> CancelT m1 Bool -> CancelT m2 Bool
asyncAnd' = undefined  

type ROMemo g s a b = Memo (Ef NP g NF NB) s a b

subtype :: ROMemo g s (Type,Type) Bool -> Type -> Type -> 
             CancelT (Par (Ef p G f b) s) Bool
subtype mem s t = 
  case (s,t) of
    (Pair s1 s2, Pair t1 t2) -> 
       -- getMemoRO mem (s1, t1) -- type checks
       asyncAnd' (lift$ getMemoRO mem (s1, t1))
                 (lift$ getMemoRO mem (s2, t2))

data Type = Pair Type Type | TInt
  deriving (Show,Read,Eq,Ord)

--------------------------------------------------------------------------------

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
   (GetEffects mOut ~ (Ef p g f b),         
    mNoGet ~ (SetEffects (Ef p NG f b) mOut), 
    LVarMonad mOut, LVarMonad mNoGet) => 
   (forall s2 mFresh . (LVarMonad mFresh, mFresh ~ (SetSession s2 mOut)) =>
          (forall a . mNoGet a -> DeadlockT mFresh a)
          -> DeadlockT mFresh res) ->
   -- TODO: could erase the gets on the way out, since none will escape:
   mOut (Maybe res)

runTillDeadlockTWW = undefined

data IMap k s v = IMap

newEmptyMap :: Par e s (IMap k s v)
newEmptyMap = undefined

insert :: (Ord k , Eq v) =>
          k -> v -> IMap k s v -> Par (Ef P g f b) s () 
insert = undefined

getKey :: Ord k => k -> IMap k s v -> Par (Ef p G f b) s v
getKey = undefined

t :: Par (Ef P g f b) s1 (Maybe Int)
t = do 
  mp <- newEmptyMap
  insert "hmm" "ok" mp -- Without this I get a perverse and strange error:
  runTillDeadlockTWW $ \ esc -> do
        esc $ do insert "hi" "there" mp
                 -- getKey "hi" mp -- Example error.  Good localization.
                 return ()
        return 3


test :: IVar s String -> Par (Ef P G f b) s String
test iv = do put iv "hi"
             get iv

test2 :: (HasPut e, HasFreeze e) => Par e s (Maybe String) 
test2 = do iv <- new
           put iv "hi"
           freeze iv

test3 :: Par (Ef P G f b) s String
test3 = do iv <- new
           put iv "hi"
           get iv

_ = runPar test3

-- _ = runPar test2
-- Couldn't match type 'F with 'NF

------------------------------------------------------------

type ReadOnly m g = (GetEffects m ~ Ef NP g NF NB)
type ReadOnly2 m = (forall g . GetEffects m ~ Ef NP g NF NB) -- TESTING

type ReadOnly3 m = (GetP (GetEffects m) ~ NP ,
                    GetB (GetEffects m) ~ NB ,
                    GetF (GetEffects m) ~ NF)
                    
type HasPut e = (GetP e ~ P)
type HasFreeze e = (GetF e ~ F)

type NoFreeze e = (NF ~ GetF e)

--------------------------------------------------------------------------------

main = putStrLn "hi"


