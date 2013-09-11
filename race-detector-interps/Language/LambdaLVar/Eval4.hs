{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveGeneric #-}

--------------------------------------------------------------------------------
-- VER 4: Refactoring of ver 3... in this version Put/Get/PrimApp
-- don't do parallel evaluation of their arguments, they do serial.

-- VER 3 [deleted]: Add Environment-passing to previous and create an explicit
-- distinction between values and Expressions.
--------------------------------------------------------------------------------

module Language.LambdaLVar.Eval4 where 

import Language.LambdaLVar.Common
import Algebra.Lattice
import Debug.Trace (trace)
import Data.Either (partitionEithers)
import qualified Data.Set as S
import qualified Data.Map as M
import Prelude hiding (exp)
import Text.PrettyPrint (text, (<>), (<+>))
import Text.PrettyPrint.GenericPretty (Out(..), doc)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- This interpreter needs a number of extra types not defined in Common:

data Thread d = 
     -- An expression to evaluate; its result will be needed:
     ThreadValue (TID d) (Exp d) (Env d) 
     -- A blocked thread, waiting for two other computations to complete:
   | ThreadBlocked (Val d -> Val d -> RunState d -> Result d) (TID d, TID d)
     -- An enabled computation, which will be executed just for effect:
   | ThreadEffect (RunState d -> Result d) -- Type aliases, simply for readability:

-- | Explicit representation of values, distinct from expressions.
data Val d = 
    VQ (QuerySet d)
  | VNum Integer
  | VLocation Var 
  | VClosure Var (Exp d) (Env d)
 deriving (Show,Eq,Ord,Generic)

instance (Show d, Out d) => Out (Val d)

-- | We provide the ability to lift values back to expressions for
-- compatibility with the other evaluators in this package:
fromVal :: Val d -> Exp d
fromVal val =
  case val of 
    VQ q           -> Q q
    VNum n         -> Num  n
    VLocation l    -> Varref l
    VClosure v b env | symMapSize env == 0 -> Lam v b      
                     | otherwise -> error "fromVal: not supporting closures with free vars"

-- | Likewise certain expressions are directly convertable to values:
toVal :: Show d => Exp d -> Val d
toVal e = 
  case e of 
    Q q -> VQ q
    Lam v b -> VClosure v b symMapEmpty -- TODO - check for free vars.
    Varref l | isLocation l -> VLocation l
    _ -> error$"toVal: expression is not a value: "++show e
    
instance Show d => Show (Thread d) where
  show (ThreadValue tid e env) = "ThreadValue "++show tid++" "++show e++" "++show env
  show (ThreadBlocked _fn tids) = "ThreadBlocked <fun> "++show tids
  show (ThreadEffect _fn)       = "ThreadEffect <fun>"

----------------------------------------
-- `RunState` is the global state of the system at any point in time:
type RunState d = (Store d, Completed d, Runnable d)
type Completed d = M.Map (TID d) (Val d)
type Runnable d = [Thread d]
type TID d = Exp d -- Thread identifiers are just expressions.
type Result d   = (Val d, RunState d)
type Env d   = SymbolMap (Val d)
type Cont    d = RunState d -> Val d -> Result d
type GetCont d = d -> Maybe (Thread d)
type Interp d = (S.Set d -> Exp d)

data Store d = Store (SymbolMap (d, [GetCont d]))
instance Show d => Show (Store d) where 
  show (Store amap) = show (stripMap amap)
instance Out d => Out (Store d) where 
  doc (Store amap) = text "Store" <+> doc (stripMap amap)
  docPrec _ x = doc x
stripMap mp = symMapMap (\ (x,ls) -> (x, length ls)) mp 
----------------------------------------

-- Note - the RunState should be used LINEARLY:

evalThreaded :: forall d . (Eq d, Show d, BoundedJoinSemiLattice d, Ord d, Out d) 
             => Exp d -> Interp d -> (Exp d, SymbolMap d)
evalThreaded eOrig interp = (fromVal finalVal, symMapMap fst finalStore)
 where
  (finalVal, (Store finalStore,_,[])) = evalloop eOrig symMapEmpty initState idCont 
  initState = (Store symMapEmpty, M.empty, [])
  
  idCont st@(_,_,[]) exp = (exp,st)
  idCont _ _ =  error "Runnables should be empty at the end of execution"
    
  threadDispatch :: RunState d -> Result d
  threadDispatch st@(_, _, []) = (error "threadDispatch had no return value", st)
  threadDispatch (store, completed, thead:rest) = 
    maytrace (show$text"  threadDispatch2: "<> doc store <> text"    "
              <> doc(M.size completed) <+> doc(1 + length rest)) $
    case thead of 
     -- A thread that's ready to run:
     ThreadValue tid exp env -> 
      -- When the thread finishes we add its result to the completed list:
      let kont (store2,completed2,runnable2) result = 
            threadDispatch (store2, 
                            M.insert tid result completed2, 
                            runnable2)
      in evalloop exp env (store, completed, rest) kont 

     -- This thread depends on two others.  It is assumed that nothing
     -- blocks on these in turn:
     ThreadBlocked fn (tid1,tid2) -> 
        -- If both of the child computations are finished, this can run:
        case (M.lookup tid1 completed, 
	      M.lookup tid2 completed) of 
	  (Just x, Just y) ->  fn x y (store, completed, rest)
          -- Otherwise go to the back of the line:
	  _ -> threadDispatch (store, completed, rest ++ [thead])
     
     ThreadEffect fn -> 
       maytrace " * Executing ThreadEFfect!! "$
       fn (store, completed, rest)

  -- Performs a highly unfair parallel evaluation.  `evalloop` maintains a
  -- list of runnable threads, but the quantum for a thread ends only
  -- when it blocks.  This is CPS'd so that it can capture the
  -- continuation at the point of a blocking get.
  evalloop :: Exp d -> Env d -> RunState d -> Cont d -> Result d
  evalloop e env state@(store@(Store amap), completed, runnable) kont =
    maytrace (show$text"EVALLOOP4: "<+> doc env <+> doc store <>text"    "<> doc e 
           <+> doc(M.size completed) <+> doc(length runnable)) $ 
    case e of 
      Q q      -> kont state (VQ q)
      Num n    -> kont state (VNum n) 
      Lam v b  -> kont state (VClosure v b env)
      Varref v ->
        case symMapLookup v env of
          Nothing -> error$"unbound variable: " ++ show v
          Just val -> kont state val

      -- Parallel evaluation contexts:
      App e1 e2 -> 
         -- Spawn threads to evaluate two expressions in parallel.
         -- Whenever both of those are completed, then reactivate the
         -- continuation (i.e. after the join).
         -- Here we use raw exps as thread IDS, which is *INCREDIBLY* inefficient:
         let tid1 = e1; tid2 = e2
             -- Create two threads that don't sync on anything else,
             -- and a third thread that is waiting on the other two:
             t1 = ThreadValue tid1 e1 env
             t2 = ThreadValue tid2 e2 env 
             t3 = ThreadBlocked doApp (tid1,tid2)
             
             -- Note that this captures the env / kont arguments:
             doApp (VClosure v bod env) v2 st = 
               let env' = symMapInsert v v2 env in
               evalloop bod env' st kont 
             doApp e1 e2 _ = error$"Bad operator/rand in application: "++show e1++" "++show e2
         in 
           -- We could jump directly to evaluating e1, but we go through the thread dispatch instead:
           threadDispatch (store, completed, t1 : t2 : t3 : runnable)
    
      -- ASSUMPTION: You'd better not call Interp on an Oracle function.
      Reify e1 -> 
        evalloop e1 env  state $ 
        \ runstate (VQ (QS ls)) -> kont runstate (toVal$ interp ls)
        
      Consume e1 -> 
         evalloop e1 env state
         (\ (Store amap,c,r) (VLocation l) -> 
           -- Overwrite the existing entry:
           let amap' = symMapInsert l (probation l,[]) amap 
               runstate' = (Store amap',c,r) 
               probation l = error$ "Probationary value for location "++show l++" touched!" in
           case symMapLookup l amap of       
	     Nothing     -> kont runstate' (VQ$ QS$ S.singleton bottom)
	     Just (x,[]) -> kont runstate' (VQ$ QS$ S.singleton x)
             Just (_,ls) -> error$"consumed value on which "++show(length ls)++" gets were still blocked"
         )

      -- Global shared memory => globally unique labels.
      -- For now using Varrefs to represent labels:
      New -> let fresh = var$ "l"++show (symMapSize amap) in
	     kont (Store$ symMapInsert fresh (bottom,[]) amap,
                  completed, runnable)
                  (VLocation fresh) 
             
      Put e1 e2 -> 
        evalloop e1 env state  $ \ state1 v1 -> 
        evalloop e2 env state1 $ \ state2 v2 ->
         let (Store amap, completed, runnable) = state2 in 
         case (v1,v2) of
          (VLocation l, VQ (QS set)) ->             
            case S.toList set of 
              [d] -> let (new, waitlist) =
                           case symMapLookup l amap of
                             Nothing -> (d,[])
                             Just (x,waitlist) -> (join d x, waitlist)
                         -- When we call each entry in the waitlist, it
                         -- will either wake-up (creating a new thread) or
                         -- stay in the waitlist:
                         (stillWaiting,thrds) = partitionEithers $
                                       map (\f -> case f new of 
                                                   Nothing  -> Left f 
                                                   Just thr -> Right thr) 
                                           waitlist 
                         -- Add the information from this put into the store:
                         store' = Store$ symMapInsert l (new,stillWaiting) amap
                         -- Wake anything ready in the waitlist:
                         runnable' = thrds ++ runnable
                     in 
                        kont (store', completed, runnable') (VQ$QS S.empty)
              _ -> error$"put can only take a singleton query set, not: "++show set
          (e1, e2) -> error$ "Bad put arguments: "++show e1++" "++show e2
        
      Get e1 e2 -> 
        evalloop e1 env state  $ \ state1 v1 -> 
        evalloop e2 env state1 $ \ state2 v2 ->
         let (Store amap, completed, runnable) = state2 in
         case (v1,v2) of
           (VLocation l, VQ quer) ->
             case symMapLookup l amap of 
               Nothing -> error$"get: Unbound store location!: "++show l
               Just (d,waiting) -> 
                  case checkReady d of
                    Just q -> kont state (VQ$QS$ S.singleton q) -- Victory!
                    Nothing ->      
                       -- It's not above us yet, capture continuation and place it in the store:
                       -- Continue processing another thread:
                       threadDispatch (Store$ symMapInsert l (d, putcont : waiting) amap,
                                       completed, runnable)
              -- Here we may need to add a new continuation to the waitlist.
              -- That continuation is invoked whenever there is the
              -- *possibility* of a wakeup -- it's not a guarantee.
              where 
               -- Function invoked by put:
               putcont newd = case checkReady newd of 
                                Nothing -> Nothing
                                Just q  -> Just$ ThreadEffect (\ state -> kont state (VQ$QS$ S.singleton q))

               checkReady d =  -- Captures 'quer':
                 case quer of 
                   QS set ->
                     -- Find all the elements of our query set less than the current state:
                     case filter (`joinLeq` d) (S.toList set) of
                      []  -> Nothing
                      [q] -> Just q
                      ls  -> error$"Uniqueness criteria failed, compatible members of query set: "++show ls               
                   QF fn -> fn d

           (e1, e2) -> error$ "Bad get arguments: "++show e1++" "++show e2

      PrimApp pr e1 e2 -> 
        evalloop e1 env state  $ \ state1 v1 -> 
        evalloop e2 env state1 $ \ state2 v2 -> 
        case (v1,v2) of
          (VNum a, VNum b) -> case pr of Add  -> kont state (VNum $ a + b)
                                         Mult -> kont state (VNum $ a * b)
          args -> error$ "Bad inputs to PrimApp: "++show args

      Unique -> error$ "Eval4 cannot handle Unique.  Desugar it first!"
