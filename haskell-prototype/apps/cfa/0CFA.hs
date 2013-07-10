{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

-- Translated from Matt Might's article: http://matt.might.net/articles/implementation-of-kcfa-and-0cfa/k-CFA.scm
-- Extended with less ad-hoc support for halting

module Main where

import Control.Applicative (liftA2, liftA3)
import qualified Control.Monad.State as State
import Control.Monad

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List ((\\))

import Debug.Trace
import Text.PrettyPrint as PP
import Text.PrettyPrint.GenericPretty (Out(doc,docPrec), Generic)

import CFA_Common

--------------------------------------------------------------------------------

-- Abstract state space
data State = State Call BEnv Store Time          deriving (Eq, Ord, Show, Generic)
-- A binding environment maps variables to addresses
-- (In Matt's example, this mapped to Addr, but I found this a bit redundant
-- since the Var in the Addr can be inferred, so I map straight to Time)
type BEnv = M.Map Var Time
-- A store maps addresses to denotable values
type Store = M.Map Addr Denotable
-- | An abstact denotable value is a set of possible values
type Denotable = S.Set Value
-- For pure CPS, closures are the only kind of value
type Value = Clo
-- Closures pair a lambda-term with a binding environment that determines
-- the values of its free variables
data Clo = Closure (Label, [Var], Call) BEnv | HaltClosure | Arbitrary deriving (Eq, Ord, Show, Generic)
-- Addresses can point to values in the store. In pure CPS, the only kind of addresses are bindings
type Addr = Bind
-- A binding is minted each time a variable gets bound to a value
data Bind = Binding Var Time                   deriving (Eq, Ord, Show, Generic)
-- In k-CFA, time is a bounded memory of program history.
-- In particular, it is the last k call sites through which
-- the program has traversed.
type Time = [Label]

-- instance Out BEnv
-- instance Out Store
instance Out Clo
instance Out Bind

instance (Out a, Out b) => Out (M.Map a b) where
  doc = docPrec 0
  docPrec _ mp = doc (M.toList mp)

instance (Out a) => Out (S.Set a) where
  doc = docPrec 0
  docPrec _ st = doc (S.toList st)

instance Out State where
  doc = docPrec 0
  docPrec _ (State call benv str time) = 
   PP.text "State" <+> doc call 
                   <+> doc benv 
--                   <+> doc str
                   <+> doc time

storeInsert :: Addr -> Value -> Store -> Store
storeInsert a v s = M.insertWith S.union a (S.singleton v) s

storeJoin :: Store -> Store -> Store
storeJoin = M.unionWith S.union

-- k-CFA parameters

k :: Int
k = 1

tick :: Label -> Time -> Time
tick l t = take k (l:t)

-- k-CFA abstract interpreter

atomEval :: BEnv -> Store -> Exp -> Denotable
atomEval benv store Halt    = S.singleton HaltClosure
atomEval benv store (Ref x) = case M.lookup x benv of
    Nothing   -> error $ "Variable unbound in BEnv: " ++ show x
    Just t -> case M.lookup (Binding x t) store of
        Nothing -> error $ "Address unbound in Store: " ++ show (Binding x t)
        Just d  -> d
atomEval benv _     (Lam l v c) = S.singleton (Closure (l, v, c) benv)

next :: State -> S.Set State -- Next states
next s@(State (Call l fun args) benv store time)
  = trace ("next " ++ show (doc s)) $
    S.fromList [ state'
               | clo <- S.toList procs
               , state' <- case clo of
                    HaltClosure -> []
                    Closure (_, formals, call') benv'
                      | let benv'' = foldr (\formal benv' -> M.insert formal time benv') benv' formals
                      -> [ State call' benv'' store' time'
                         | params <- S.toList (transpose paramss)
                         , let store' = foldr (\(formal, params) store  -> storeInsert (Binding formal time) params store) store (formals `zip` params)
                         ]
                    Arbitrary
                      -> [ state'
                         | params <- S.toList (transpose paramss)
                         , param <- params
                         , Just state' <- [escape param store]
                         ]
               ]
  where time' = tick l time
        procs  = atomEval benv store fun
        paramss = map (atomEval benv store) args

-- Extension of my own design to allow CFA in the presence of arbitrary values.
-- Similar to "sub-0CFA" where locations are inferred to either have either a single
-- lambda flow to them, no lambdas, or all lambdas
escape :: Value -> Store -> Maybe State
escape Arbitrary                          _     = Nothing -- If an arbitrary value from outside escapes we don't care
escape HaltClosure                        _     = Nothing
escape (Closure (_l, formals, call) benv) store = Just (State call (benv `M.union` benv') (store `storeJoin` store') [])
  where (benv', store') = fvStuff formals

fvStuff :: [Var] -> (BEnv, Store)
fvStuff xs = (M.fromList [(x, []) | x <- xs], M.fromList [(Binding x [], S.singleton Arbitrary) | x <- xs])

transpose :: Ord a => [S.Set a] -> S.Set [a]
transpose []         = S.singleton []
transpose (arg:args) = S.fromList [arg:args | args <- S.toList (transpose args), arg <- S.toList arg]

-- State-space exploration

explore :: S.Set State -> [State] -> S.Set State
explore seen [] = seen
explore seen (todo:todos)
  | todo `S.member` seen = explore seen todos
  | otherwise            = explore (S.insert todo seen) (S.toList (next todo) ++ todos)
 -- NB: Might's dissertation (Section 5.3.5) explains how we can apply widening here to
 -- improve the worst case runtime from exponential to cubic: for an new state from the
 -- work list, we must extract all seen states which match in every element *except* the
 -- store. Then, join those seen stores together. If the potential store is a subset
 -- of the seen ones then we can just loop. Otherwise, union the new store onto a global
 -- "widening" store, update the global store with this one, and do abstract evalution on the state with the new sotre.

-- User interface

summarize :: S.Set State -> Store
summarize states = S.fold (\(State _ _ store' _) store -> store `storeJoin` store') M.empty states

-- ("Monovariant" because it throws away information we know about what time things arrive at)
monovariantStore :: Store -> M.Map Var (S.Set Exp)
monovariantStore store = M.foldrWithKey (\(Binding x _) d res -> M.alter (\mb_exp -> Just $ maybe id S.union mb_exp (S.map monovariantValue d)) x res) M.empty store

monovariantValue :: Value -> Exp
monovariantValue (Closure (l, v, c) _) = Lam l v c
monovariantValue HaltClosure           = Halt
monovariantValue Arbitrary             = Ref "unknown"

analyse :: Call -> M.Map Var (S.Set Exp)
analyse e = monovariantStore (summarize (explore S.empty [State e benv store []]))
  where (benv, store) = fvStuff (S.toList (fvsCall e))

fvsCall :: Call -> S.Set Var
fvsCall (Call _ fun args) = fvsExp fun `S.union` S.unions (map fvsExp args)

fvsExp :: Exp -> S.Set Var
fvsExp Halt         = S.empty
fvsExp (Ref x)      = S.singleton x
fvsExp (Lam _ xs c) = fvsCall c S.\\ S.fromList xs

------------------------------------------------------------------------------------------

main = forM_ [fvExample, standardExample] $ \example -> do
-- main = forM_ [standardExample] $ \example -> do
         putStrLn "====="
         forM_ (M.toList (analyse (runUniqM example))) $ \(x, es) -> do
           putStrLn (x ++ ":")
           mapM_ (putStrLn . ("  " ++) . show) (S.toList es)
