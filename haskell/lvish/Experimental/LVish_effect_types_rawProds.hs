{-# LANGUAGE DataKinds #-}  -- For Determinism
{-# LANGUAGE KindSignatures #-}  -- For Determinism
{-# LANGUAGE GADTs #-}  -- For Determinism
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

import Control.Monad
import Control.Applicative 
import Control.Monad.Trans.Class

-- APPROACH: Expose type level products explicitly in all methods.

--------------------------------------------------------------------------------
-- Dummy definitions:

class ParMonad (m :: * -> *) where
class LVarMonad (m :: * -> *) where
data Determinism = Det | QuasiDet deriving Show

newtype Par :: EffectsSig -> * -> * -> * where
  WrapPar :: Double -> Par d s a
instance Monad (Par efs s) where
  (>>=) = undefined
  return = undefined

instance LVarMonad (Par efs s) where

data IVar s a = IVar 
data LVar s a = LVar 
data CancelT   (p:: * -> *) a   = CancelT
data DeadlockT (p:: * -> *) a = DeadlockT

instance Monad m => Monad (CancelT m) where
instance ParMonad m => ParMonad (CancelT m) where
instance LVarMonad m => LVarMonad (CancelT m) where
instance Monad m => Monad (DeadlockT m) where
instance ParMonad m => ParMonad (DeadlockT m) where
instance LVarMonad m => LVarMonad (DeadlockT m) where

instance MonadTrans CancelT where
instance MonadTrans DeadlockT where

data TID = TID

--------------------------------------------------------------------------------
-- Effect sigs and their extraction:
--------------------------------------------------------------------------------

type family GetEffects (m :: (* -> *)) :: EffectsSig
type instance GetEffects (Par e s) = e
type instance GetEffects (CancelT m) = GetEffects m
type instance GetEffects (DeadlockT m) = GetEffects m

type family GetSession (m :: (* -> *)) :: *
type instance GetSession (Par e s) = s
type instance GetSession (CancelT m)   = GetSession m
type instance GetSession (DeadlockT m) = GetSession m

type family SetSession (s :: *) (m :: (* -> *)) :: (* -> *)
type instance SetSession s2 (Par e s) = Par e s2
type instance SetSession e (CancelT m)   = CancelT   (SetSession e m)
type instance SetSession e (DeadlockT m) = DeadlockT (SetSession e m)

type family SetEffects (e::EffectsSig) (m :: (* -> *)) :: (* -> *)
type instance SetEffects e2 (Par e1 s) = Par e2 s
type instance SetEffects e (CancelT m)   = CancelT   (SetEffects e m)
type instance SetEffects e (DeadlockT m) = DeadlockT (SetEffects e m)

data EffectsSig  = Ef Putting Getting Freezing Bumping
data Putting  = P | NP
data Getting  = G | NG
data Freezing = F | NF
data Bumping  = B | NB

--------------------------------------------------------------------------------
-- Core ops:

put :: IVar s a -> a -> Par (Ef P g f b) s ()
put = undefined

get :: IVar s a -> Par (Ef p G f b) s a
get = undefined

new :: Par e s (IVar s a)
new = undefined

runCancelT :: CancelT (Par (Ef NP g NF NB) s) a -> Par (Ef p g f b) s a
runCancelT = undefined

forkCancelable :: (ParMonad m, ReadOnly m _g) =>
                   CancelT m () -> CancelT m TID
forkCancelable = undefined

cancel :: ParMonad m => TID -> CancelT m ()
cancel = undefined

--------------------------------------------------------------------------------
-- Memoization:


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

runParRO :: (forall s . Par (Ef NP g NF NB) s a) -> a
runParRO = undefined

runPar :: (forall s . Par (Ef p g NF b) s a) -> a
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

insert :: (Ord k, Eq v) =>
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
-- The getKey error above has bad locality in this version:
-- LVish_effect_types2.hs:122:3:
--     Couldn't match type 'G with 'NG
--     Expected type: 'Ef 'P 'NG f b
--       Actual type: GetEffects (Par ('Ef 'P 'G f b) s1)
--     In the expression: runTillDeadlockWithWrites     


-- test :: HasPut e => IVar s String -> Par e s String
test iv = do put iv "hi"
             get iv

-- test2 :: HasPut e => Par e s String
-- test2 :: (HasPut e, HasGet e) => Par e s String
test2 = do iv <- new
           put iv "hi"
           get iv

test3 :: Par (Ef P G f b) s String
test3 = do iv <- new
           put iv "hi"
           get iv
------------------------------------------------------------


type ReadOnly m g = (GetEffects m ~ Ef NP g NF NB)
type ReadOnly2 m = (forall g . GetEffects m ~ Ef NP g NF NB) -- TESTING

--------------------------------------------------------------------------------

main = putStrLn "hi"


