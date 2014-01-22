{-# LANGUAGE DataKinds #-}  -- For Determinism
{-# LANGUAGE KindSignatures #-}  -- For Determinism
{-# LANGUAGE GADTs #-}  -- For Determinism
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
-- {-# LANGUAGE ImpredicativeTypes #-}

import Control.Monad
import Control.Applicative 
import Control.Monad.Trans.Class

-- APPROACH: Attempt to hide type-level products behind "Has" constraints.

--------------------------------------------------------------------------------

#include "common.hs"

{-
data Determinism = Det | QuasiDet 
  deriving Show

-- newtype Par :: Determinism -> * -> * -> * where
newtype Par :: EffectsSig -> * -> * -> * where
  WrapPar :: Double -> Par d s a
instance Monad (Par efs s) where
  (>>=) = undefined
  return = undefined

data IVar s a = IVar 
data LVar s a = LVar 
data CancelT   (p:: * -> *) a   = CancelT
data DeadlockT (p:: * -> *) a = DeadlockT

instance Monad m => Monad (CancelT m) where
instance Monad m => Monad (DeadlockT m) where

instance MonadTrans DeadlockT where

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
-- data IOing  = YesIO   | NoIO

-}



put :: HasPut e => IVar s a -> a -> Par e s ()
put = undefined

get :: HasGet e => IVar s a -> Par e s a
get = undefined

new :: Par e s (IVar s a)
new = undefined

--------------------------------------------------------------------------------
-- Cancelation
--------------------------------------------------------------------------------

-- runCancelT :: (NoPut e, NoFreeze e) => CancelT (Par e s) a -> Par e s a
runCancelT :: (NoPutM m) => CancelT m a -> m a
runCancelT = undefined

forkCancelable :: (LVarMonad m, ReadOnlyM m) =>
                   CancelT m () -> CancelT m TID
forkCancelable = undefined

runParRO :: NoPut e => (forall s . Par e s a) -> a
runParRO = undefined

-- runPar :: IsDet e => (forall s . Par e s a) -> a
runPar :: NoFreeze e => (forall s . Par e s a) -> a
runPar = undefined

runTillDeadlock :: (forall s . DeadlockT (Par e s) a) -> Par e s (Maybe a)
runTillDeadlock = undefined

-- runTillDeadlockWithWrites :: NoGet e2 => 
--   (forall s1 .
-- --    (Par e2 s2 a -> DeadlockT (Par e s) a)
--    (DeadlockT (Par e2 s2) a -> DeadlockT (Par e2 s1) a) -- ^ Lifter function.
--    -> DeadlockT (Par e s1) b)
--   -> Par e s (Maybe b)

-- We can perform put or freeze operation to the world outside a `runTillDeadlock`,
-- but we cannot do blocking reads.
runTillDeadlockWithWrites :: 
  (forall s2 .
   (DeadlockT (Par (Ef p NG f b) s1) a -> 
    DeadlockT (Par e1 s2) a) -- ^ Lifter function.
   -> DeadlockT (Par e1 s2) res)
  -> Par e1 s1 (Maybe res)
  -- (forall s2 .
  --  (DeadlockT (Par (Ef p NG f b) s1) a -> 
  --   DeadlockT (Par (Ef p  g f b) s2) a) -- ^ Lifter function.
  --  -> DeadlockT (Par (Ef p g f b) s2) res)
  -- -> Par e s1 (Maybe res)

runTillDeadlockWithWrites = undefined

data IMap k s v = IMap

newEmptyMap :: Par e s (IMap k s v)
newEmptyMap = undefined

insert :: (Ord k, Eq v) =>
          k -> v -> IMap k s v -> Par (Ef P g f b) s () 
insert = undefined

getKey :: Ord k => k -> IMap k s v -> Par (Ef p G f b) s v
getKey = undefined

-- t :: NoGet e => Par e s1 (Maybe Int)
t :: Par e s1 (Maybe Int)
t = do 
  mp <- newEmptyMap
  runTillDeadlockWithWrites $ \ lifter -> do
     lifter $ do lift$ insert "hi" "there" mp
                 -- lift$ getKey "hi" mp
                 return ()
     return 3

--------------------
-- Inferred type:
  -- :: Monad (Par ('Ef 'P f b) s) =>
  --    IVar * * s [Char] -> Par ('Ef 'P f b) s [Char]
----------------------------
--Attempting a bad signature:
-- test :: IVar s String -> Par ReadOnly s String
-- Gives this error:
    -- Couldn't match type 'Ef 'P f0 b0
    --               with `forall (f :: Freezing) (b :: Bumping). 'Ef 'NP f b'
    -- Expected type: Par ReadOnly s ()
    --   Actual type: Par ('Ef 'P f0 b0) s ()
----------------------------
-- Correct signature:
test :: IVar s String -> Par (Ef P G f b) s String
-- test :: IVar s String -> Par DoesWrite s String
test iv = do put iv "hi"
             get iv

test2 :: (HasPut e, HasGet e) => Par e s String
test2 = do iv <- new
           put iv "hi"
           get iv

test3 :: String
-- test3 = runPar test2
test3 = runPar (test2 :: Par (Ef P G NF NB) s String)

------------------------------------------------------------
-- Instances:

class HasGet (e :: EffectsSig) where
instance HasGet (Ef p G f b)

class HasPut (e :: EffectsSig) where
instance HasPut (Ef P g f b)

type HasPutM m = (HasPut (GetEffects m))
type HasGetM m = (HasGet (GetEffects m))

type NoPutM m    = (NoPut    (GetEffects m))
type NoFreezeM m = (NoFreeze (GetEffects m))
type NoBumpM m   = (NoBump   (GetEffects m))

type ReadOnlyM m  = (NoPutM m, NoBumpM m, NoFreezeM m)
type ReadOnly e  = (NoPut e, NoBump e, NoFreeze e)

class NoPut (e :: EffectsSig) where
instance NoPut (Ef NP g f b)

class NoFreeze (e :: EffectsSig) where
instance NoFreeze (Ef p g NF b)

instance NoBump (Ef p g NF b)

class NoGet (e :: EffectsSig) where
instance NoBump (Ef p NG f b)

class NoBump (e :: EffectsSig) where
instance NoBump (Ef p g f NB)

-- | This is tricky because its an OR
class IsDet (e :: EffectsSig) where 
instance IsDet (Ef p g NF b) where 
instance IsDet (Ef NP g f b) where


{-

-- class IsDet (d :: EffectsSig a b c) where
class IsDet (d :: EffectsSig) where
instance IsDet (MkEffProd p NoFreeze b) where 
instance IsDet (MkEffProd NoPut f b) where

class DoesPut (efs :: EffectsSig) where 
instance DoesPut (MkEffProd Put f ni) where

class DontPut (efs :: EffectsSig) where 
instance DontPut (MkEffProd NoPut f ni) where

  
class DoesFreeze (efs :: EffectsSig) where 
instance DoesFreeze (MkEffProd p Freeze ni) where 
  
----------------------------------------
    
put :: DoesPut d => LVar s a -> a -> Par d s ()
put = undefined

freeze :: DoesFreeze d => LVar s a -> a -> Par d s ()
freeze = undefined

p1 :: DoesPut d => Par d s ()
p1 = put (undefined::LVar s Int) (3::Int)

p2 :: DoesFreeze d => Par d s ()
p2 = freeze (undefined::LVar s Int) 3

p3 :: (DoesPut d, DoesFreeze d) => Par d s ()
p3 = do p1; p2

p4 :: Par (MkEffProd Put Freeze ni) s ()
p4 = p3

-- p5 :: Par (MkEffProd NoPut Freeze ni) s ()
-- p5 = p4

-- p1 :: Par 
  
-- put :: DoesPut d => LVar s a -> a -> Par d s aa

-- We'd need an indexed monad with a special bind... needs rebindable
-- 'do' syntax.

-}

--------------------------------------------------------------------------------
#if 0
data Effect = UsesFreeze
            | UsesPut
            | NonIdemp
  deriving Show

newtype Par2 :: [Effect] -> * -> * -> * where
  WrapPar2 :: Double -> Par2 efs s a

y :: Par2 [UsesPut, UsesFreeze] s Int
y = undefined

z :: Par2 [UsesFreeze, UsesPut] s Int
z = undefined

class DoesPut (x :: [Effect]) where

instance DoesPut (UsesPut ': tail) where
instance DoesPut xs => DoesPut (hd ': xs) where
                  
-- instance DoesPut ((:) Effect 'UsesPut tail) where  
-- foo :: DoesPut x => x -> Int
-- foo = undefined
-- _ = foo y
-- _ = foo z 
#endif


main = putStrLn "hi"

