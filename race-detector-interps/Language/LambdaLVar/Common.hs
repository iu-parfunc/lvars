{-# LANGUAGE GADTs, StandaloneDeriving, ScopedTypeVariables, CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverlappingInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- | Common pieces shared by the various LambdaLVar interpreters.

module Language.LambdaLVar.Common 
--       ()
       where

-- From 'lattices' package:  Classes for join semi-lattices, top, bottom:
import Algebra.Lattice (BoundedJoinSemiLattice(..), JoinSemiLattice(..))
import qualified Data.Set as S
import Debug.Trace (trace)
import Text.PrettyPrint (text, (<>), cat, sep, braces, comma, hsep)
import Text.PrettyPrint.GenericPretty
import GHC.Generics
import Data.List as L (intersperse, lookup) 
import Prelude as P

import System.Environment (getEnvironment)
import System.IO.Unsafe   (unsafePerformIO)

--------------------------------------------------------------------------------
-- Prelude: Pick a simple representation of variables (interned symbols)
--------------------------------------------------------------------------------
-- Several modules offer this, with varying problems:
----------------------------
#define USE_SYMBOL
#ifdef USE_STRINGTABLE
-- 'stringtable-atom' package:
-- I'm getting some segfaults here [2012.05.19];
import StringTable.Atom
import StringTable.AtomMap as AM
var = toAtom
type Var = Atom
type SymbolMap a = AtomMap a 
symMapToList = AM.toList
symMapFromList = AM.fromList
symMapInsert = AM.insert
symMapLookup = AM.lookup
symMapMap    = AM.map 
symMapEmpty  = AM.empty
symMapSize   = AM.size
symMapElems  = AM.elems
----------------------------
#elif defined(USE_SIMPLEATOM)
-- 'simple-atom' package:
import Data.Atom.Simple
var = intern
type Var = Symbol
----------------------------
#elif defined(USE_SYMBOL)
-- 'symbol' package
import Data.Symbol
import Data.Map as M
var = intern
type Var = Symbol 
-- instance Show Symbol where 
--  show = unintern
-- instance Read Symbol where 
type SymbolMap a = M.Map Symbol a
-- NOTE - this package would seem to be unsafe because the Symbol type
-- constructor is exported.
symMapToList = M.toList
symMapFromList = M.fromList
symMapInsert k v m = M.insert k v m
symMapLookup k m   = M.lookup k m 
symMapMap    f m   = M.map    f m
symMapEmpty  = M.empty
symMapSize   = M.size 
symMapElems  = M.elems
#endif
  
var :: String -> Var
symMapFromList :: [(Var,a)] -> SymbolMap a 

--------------------------------------------------------------------------------
-- * IVar lattices, no ordering on contents, top omitted:
data IVar a = Empty 
            | Full a 
            | Top
  deriving (Show, Eq, Ord, Generic)
-- deriving instance Show a => Show (IVar a)
-- deriving instance Eq   a => Eq   (IVar a)
-- deriving instance Ord  a => Ord  (IVar a) -- NOT the lattice ordering in this case.

instance Eq a => JoinSemiLattice (IVar a) where 
  join a Empty = a
  join Empty b = b
  join Top _ = Top
  join _ Top = Top
  join (Full a) (Full b) | a == b    = Full a
			 | otherwise = Top

instance Eq a => BoundedJoinSemiLattice (IVar a) where 
  bottom = Empty

-- Helpers for query literals.  
-- This one is for Get'ing a full IVar with one of a set of possible values:
fullIvarQ :: (Ord a, Show a) => [a] -> Exp (IVar a)
fullIvarQ ls = Q$ QS $ S.fromList $ P.map Full ls

--------------------------------------------------------------------------------
-- * String-append lattices for DEBUGGING:
-- These have a non-commutative LUB, so they're CHEATING

data StrLat = LEmpty
            | LStr String
            | LTop
  deriving (Show, Eq, Ord, Generic)

instance JoinSemiLattice StrLat where 
  join a LEmpty = a
  join LEmpty b = b
  join LTop _ = LTop
  join _ LTop = LTop
  join (LStr a) (LStr b) = LStr (a++b)

instance BoundedJoinSemiLattice StrLat where 
  bottom = LEmpty

--------------------------------------------------------------------------------
-- * LRJ lattices for pedigrees - these are also a cheat since they're
--   non-commutative.

newtype LRJ = LRJ Integer
  deriving (Show, Eq, Ord, Generic, Out)

instance JoinSemiLattice LRJ where 
  -- CONVENTION -- the incoming Put should alwyas be on the LEFT:
  join (LRJ k) (LRJ n) | k == 1 || k == 2 || k == 3
      = LRJ $ 10 * n + k

  join new (LRJ 0) = new -- HACK: Can overwrite uninitialized ones...
  join x y = error$"Unexpected arguments to join for LRJ lattice: "++show (x,y)

instance BoundedJoinSemiLattice LRJ where 
  bottom = LRJ 0

-- instance Generic d => Generic (QuerySet d) where

--------------------------------------------------------------------------------
-- * Powerset lattices for counters:

-- TODO


--------------------------------------------------------------------------------
-- Grammar for Lambda par:
--------------------------------------------------------------------------------

-- Lambda-Par Expression parameterized over a domain:
data Exp d = 
    Varref Var
  | App (Exp d) (Exp d)
  | Lam Var (Exp d)      -- I don't like reading De Bruijn  
  | Q (QuerySet d) 
  | New
  | Put (Exp d) (Exp d)  -- label and singleton query set
  | Get (Exp d) (Exp d)  -- label and query set
  | Consume (Exp d)      -- label only
  | Reify  (Exp d)      -- query set
    
  | Unique  -- This can be desugared!
    
  -- These are not strictly necessary, but here we add numbers and arithmetic to make tests easier:
  | Num Integer
  | PrimApp Prim (Exp d) (Exp d)

 deriving (Show, Eq, Ord, Generic)

data Prim = Add | Mult
 deriving (Show, Eq, Ord, Generic)
          
-- type Oracle d = d -> Bool

-- | The convention with oracles is that they represent a potentially
--   infinite set of states.  When given a state as input, they will
--   return a state in the selected set that is BELOW the input state,
--   if one exists, and otherwise return Nothing.
type Oracle d = d -> Maybe d

data QuerySet d = QS (S.Set d)  -- Explicit, literal query *sets*.  This could be translated 
                                -- to a function, but then it wouldn't be printable.
                | QF (Oracle d) -- Query sets defined as predicate functions.  The onus is on the
                                -- user to ensure incompatibility between all the states that
                                -- satisfy the predicate.

instance Show d => Show (QuerySet d) where 
  show (QS s) = "QS "++ show s
  show fn = "<QPredicateFn>"
instance Eq d => Eq (QuerySet d) where 
  QS s1 == QS s2 = s1 == s2
  _ == _         = False

instance Ord d => Ord (QuerySet d) where
  compare (QS s1) (QS s2) = compare s1 s2
  compare a b = EQ 
   
--instance (Show (QuerySet d)) => Out (QuerySet d) where
instance (Show d) => Out (QuerySet d) where
  doc a       = text (show a :: String)
  docPrec n a = text (show a :: String)


------------------------------------------------------------

isValue :: Exp t -> Bool
isValue (Lam _ _)    = True
isValue (Q   _)      = True
isValue (Num   _)    = True
isValue (Varref v) | isLocation v = True
isValue _            = False

-- Is variable a location variable or a regular one.  Simple convention for now:
isLocation :: Var -> Bool
isLocation v = case show v of 
	         ('l':_) -> True 
		 _       -> False


-- | Replace free occurrences of a variable with an expression.
subst :: Exp d -> Var -> Exp d -> Exp d
subst new var (Varref v) | var == v = new
subst new var e = 
 let loop = subst new var in  
 case e of 
  Varref _ -> e
  Q      _ -> e
  Num n    -> e  
  New      -> e
  Unique   -> e  
  Lam v  b | v == var  -> e 
	   | otherwise -> Lam v (loop b)
  App e1 e2 -> App (loop e1) (loop e2)
  Put e1 e2 -> Put (loop e1) (loop e2)
  Get e1 e2 -> Get (loop e1) (loop e2)
  Consume x -> Consume (loop x)
  Reify  x -> Reify  (loop x)
  PrimApp p e1 e2 -> PrimApp p (loop e1) (loop e2)

--------------------------------------------------------------------------------
-- * Interpretation Functions
--------------------------------------------------------------------------------

-- A silly Reify function that produces strange unbound variables:
exampleReify :: Show d => S.Set d -> Exp d
exampleReify ls = Varref (var$ show ls)

--------------------------------------------------------------------------------
-- Pretty Printing
--------------------------------------------------------------------------------

instance (Show d, Out d) => Out (Exp d)

instance Out Prim where
  docPrec n a = text (show a :: String)
  doc s       = docPrec 0 s

instance Out Var where
  docPrec n a = text (show a :: String)
  doc s = docPrec 0 s

instance Out t => Out (S.Set t) where 
  docPrec _ s | S.size s == 0 = text "{}"
  docPrec _ s = braces $ sep $ 
                (P.map (<> comma) (init ls) ++ [last ls])
    where ls = P.map (docPrec 0) (S.toList s)
  doc s = docPrec 0 s

instance Out t => Out (IVar t)

instance Out t => Out (SymbolMap t) where
  docPrec _ m = 
          braces $
          cat $ 
          -- intersperse (text ", ") $ 
          P.map  (<> text ", ") $ 
          P.map (\ (x,y) -> doc x <> text ": " <> docPrec 0 y) $ 
          symMapToList m
  doc m = docPrec 0 m

--------------------------------------------------------------------------------
-- Syntax construction helpers:
--------------------------------------------------------------------------------

-- | Parallel composition
letpar :: [(Var, Exp d)] -> Exp d -> Exp d
letpar [] bod = bod
letpar binds bod = buildApp (reverse binds)  
 where buildLam [] = bod
       buildLam ((lhs,_):rest) = Lam lhs (buildLam rest)
       buildApp [] = buildLam binds
       buildApp ((_,rhs):rest) = App (buildApp rest) rhs

-- | By convention, this is our ignored/void value:
void :: Exp d
void = Q$QS S.empty

-- | Sequential composition:
-- begin ls

-- | Basic let, non curried function application (sequential evaluation):
lett :: [(Var, Exp d)] -> Exp d -> Exp d
lett [] bod = bod
lett ((lhs,rhs):rest) bod = App (Lam lhs (lett rest bod)) rhs

singQ :: d -> Exp d
singQ x = Q$ QS (S.singleton x)

-- | A CHEATING query set.  Read the exact state of what is there:
anyQ = Q (QF Just)

-- Put and get to a variable... that's usually the target!
put x = Put (Varref x)
get x = Get (Varref x)

-- Debug mode -- there are various debugging messages scattered throughout the code:
dbg :: Bool
dbg = case L.lookup "DEBUG" theENV of 
        Just "1" -> True
        Just "0" -> False
        Just ""  -> False
        Just s   -> error$ "env var DEBUG set to unknown value: "++s
        Nothing  -> False

theENV = unsafePerformIO getEnvironment

maytrace :: String -> a -> a
maytrace = if dbg then trace else (\ _ y -> y)
