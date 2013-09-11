{-# LANGUAGE GADTs, StandaloneDeriving, ScopedTypeVariables, CPP #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

--------------------------------------------------------------------------------
-- VER 1: Regular evaluator, with a rewriting flavor:
--------------------------------------------------------------------------------

module Language.LambdaLVar.Eval1 where 

import Language.LambdaLVar.Common
import Algebra.Lattice
import qualified Data.Set as S
import Text.PrettyPrint.GenericPretty (Out, doc)
import Text.PrettyPrint (text, (<>))


--------------------------------------------------------------------------------

-- | Takes an expression and an interp function.
eval :: (Eq d, Show d, BoundedJoinSemiLattice d, Out d)
     => Exp d -> (S.Set d -> Exp d) -> (Exp d, SymbolMap d)
eval eOrig interp = outerloop symMapEmpty eOrig 
 where
  outerloop store expr = 
    maytrace (show$ text"EVAL1: "<> doc store <> text"    " <> doc expr) $
    let (expr',store') = loop store expr in
    if isValue expr'   then (expr',store')
    else if expr == expr'
    then error$"No further evaluation possible, program stuck:\n "++show (doc expr)
    else outerloop store' expr' 

  -- Returns (expression,store):
  loop store e =
--    maytrace (show$ text"LOOP: "<> doc store <> text"    " <> doc e) $
    case e of 
      Q _      -> (e, store)          -- Already a value.
      Lam _ _  -> (e, store)          -- Already a value.
      Num _    -> (e, store)          -- Already a value.      
      
      Varref v | isLocation v -> (e, store) -- Allowing as a value only for LOCATIONS.
      Varref v -> error$"unbound variable: " ++ show v

      -- No further evaluation of opera* is possible in this case.
      -- Only then perform beta reduction:
      App e1 e2 | isValue e1 && isValue e2 -> 
        case e1 of
          Lam v bod -> loop store (subst e2 v bod)
	  _ -> error$"Type error, non lambda in operator position: "++ show e1

      Reify (Q (QS s)) -> (interp s, store)
      Reify (Q (QF _)) -> error "Should not call Reify on a predicate-based QuerySet"

      -- Global shared memory with global uniqueness.
      -- For now using Varrefs to represent labels:
      New -> let fresh = var$ "l"++show (symMapSize store) in
	     (Varref fresh, 
	      symMapInsert fresh bottom store)

      Get (Varref l) (Q q) -> 
        case symMapLookup l store of 
	  Nothing -> error$"get: Unbound store location!: "++show l

          -- Here we find all the elements of our query set less than the current state:
          Just d  -> 
            case q of 
              QS set -> 
                case filter (`joinLeq` d) (S.toList set) of             
                 []  -> maytrace (show$ text" GET not ready : " <> doc set <>text"  "<> doc d) $
                        (e      , store) -- It's not above us yet, return original.
                 [d] -> (singQ d, store)
                 ls  -> error$"Uniqueness criteria failed, compatible members of query set: "++show ls
              QF fn | Just d' <- fn d -> (singQ d, store)
                    | otherwise       -> (e, store) -- it does not satisfy the pred.

      Consume (Varref l) -> 
          -- Overwrite the existing entry:
          let store' = symMapInsert l (probation l) store in
          case symMapLookup l store of       
	    Nothing -> (singQ bottom, store')
	    Just x  -> (singQ   x   , store')

      Put (Varref l) (Q q) ->   
        let QS set = q in
          case S.toList set of 
	    [d] -> let new = case symMapLookup l store of       
			      Nothing -> d
			      Just x  -> join d x
                   in (void, symMapInsert l new store)
	    _ -> error$"put can only take a singleton query set, not: "++show set
		    

      PrimApp pr v1 v2 | isValue v1 && isValue v2 ->
        case (v1,v2) of 
          (Num a, Num b) -> 
            case pr of Add  -> (Num $ a + b, store)
                       Mult -> (Num $ a * b, store)
          args -> error $"Bad value arguments to numeric primitive: "++show args

      -- Here we can't reduce the form, but we can let the children take a step:
      App e1 e2 -> recur2 App e1 e2 
      Get e1 e2 -> recur2 Get e1 e2 
      Put e1 e2 -> recur2 Put e1 e2 
      Consume e -> recur1 Consume e
      Reify e   -> recur1 Reify   e
      PrimApp pr e1 e2 -> recur2 (PrimApp pr) e1 e2 
      
      Unique    -> (e,store) -- This can't be reduced, and is NOT handled by this interpreter.

    where recur2 constr e1 e2 =
	    let (e1',store' ) = loop store  e1 
		(e2',store'') = loop store' e2 
	    in  (constr e1' e2', store'')
          recur1 constr e = 
            let (e',store') = loop store e 
	    in  (constr e', store')
          probation l = error$ "Probationary value for location "++show l++" touched!"
     
