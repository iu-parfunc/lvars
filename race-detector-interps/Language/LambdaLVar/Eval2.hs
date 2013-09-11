{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-name-shadowing #-}

--------------------------------------------------------------------------------
-- VER 2: Interpreter in continuation passing style
-- This uses explicit substitution for beta reduction.
-- This version maintains an explict pool of threads.
--------------------------------------------------------------------------------

module Language.LambdaLVar.Eval2 where 

import Language.LambdaLVar.Common
import Algebra.Lattice
import Debug.Trace (trace)
import Data.Either (partitionEithers)
import qualified Data.Set as S
import qualified Data.Map as M
import Prelude hiding (exp)
import Text.PrettyPrint (text, (<>), (<+>))
import Text.PrettyPrint.GenericPretty (Out(..), doc)

--------------------------------------------------------------------------------
-- This interpreter needs a number of extra types not defined in Common:

data Thread d = 
     -- An expression to evaluate; its result will be needed:
     ThreadValue (TID d) (Exp d)
     -- A blocked thread, waiting for two other computations to complete:
   | ThreadBlocked (Val d -> Val d -> RunState d -> Result d) (TID d, TID d)
     -- An enabled computation, which will be executed just for effect:
   | ThreadEffect (RunState d -> Result d) -- Type aliases, simply for readability:

instance Show d => Show (Thread d) where
  show (ThreadValue tid e) = "ThreadValue "++show tid++" "++show e
  show (ThreadBlocked _fn tids) = "ThreadBlocked <fun> "++show tids
  show (ThreadEffect _fn)       = "ThreadEffect <fun>"

----------------------------------------
-- This is the global state of the system at any point in time:
type RunState d = (Store d, Completed d, Runnable d)
type Completed d = M.Map (TID d) (Val d)
type Runnable d = [Thread d]
type Val d = Exp d -- Values are just [a subset of] expressions.
type TID d = Exp d -- Thread identifiers are just expressions.
type Result d   = (Val d, RunState d)
type Cont    d = RunState d -> Val d -> Result d
type GetCont d = d -> Maybe (Thread d)
type Interp d = (S.Set d -> Val d)

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
evalThreaded eOrig interp = (finalVal, symMapMap fst finalStore)
  where
  (finalVal, (Store finalStore,_,[])) = evalloop eOrig idCont initState
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
     ThreadValue tid exp -> 
      -- When the thread finishes we add its result to the completed list:
      let kont (store2,completed2,runnable2) result = 
            threadDispatch (store2, 
                            M.insert tid result completed2, 
                            runnable2)
      in evalloop exp kont (store, completed, rest)

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
       maytrace " * Executing ThreadEffect!! "$
       fn (store, completed, rest)

  -- Performs a highly unfair parallel evaluation.  `evalloop` maintains a
  -- list of runnable threads, but the quantum for a thread ends only
  -- when it blocks.  This is CPS'd so that it can capture the
  -- continuation at the point of a blocking get.
  evalloop :: Exp d -> Cont d -> RunState d -> Result d
  evalloop e kont state@(store@(Store amap), completed, runnable) =
    maytrace (show$text"EVALLOOP2: "<> doc store <>text"    "<> doc e 
           <+> doc(M.size completed) <+> doc(length runnable)) $
    case e of 
      Q _      -> kont state e          -- Already a value.
      Lam _ _  -> kont state e          -- Already a value.
      Num _    -> kont state e          -- Already a value.
      Varref v | isLocation v -> kont state e -- Allowing as a value only for LOCATIONS.
      Varref v -> error$"unbound variable: " ++ show v

      Reify e1 -> 
        evalloop e1 (\ runstate (Q (QS ls)) -> kont runstate (interp ls)) state
        
      Consume e1 -> 
         evalloop e1
         (\ (Store amap,c,r) (Varref l) -> 
           -- Overwrite the existing entry:
           let amap' = symMapInsert l (probation l,[]) amap 
               runstate' = (Store amap',c,r) in
           case symMapLookup l amap of       
	     Nothing     -> kont runstate' (singQ bottom)
	     Just (x,[]) -> kont runstate' (singQ x)
             Just (_,ls) -> error$"consumed value on which "++show(length ls)++" gets were still blocked"
         ) state

      -- Global shared memory => globally unique labels.
      -- For now using Varrefs to represent labels:
      New -> let fresh = var$ "l"++show (symMapSize amap) in
	     kont (Store$ symMapInsert fresh (bottom,[]) amap,
                  completed, runnable)
                  (Varref fresh) 
             
      -- Parallel evaluation contexts:
      App        e1 e2 -> forkJoin doApp e1 e2
      Get        e1 e2 -> forkJoin doGet e1 e2
      Put        e1 e2 -> forkJoin doPut e1 e2      
      PrimApp pr e1 e2 -> forkJoin (doPrimApp pr) e1 e2 

      Unique -> error$ "Eval2 cannot handle Unique.  Desugar it first!"
      
   where 
     probation l = error$ "Probationary value for location "++show l++" touched!"
               
     -- These do* functions may use evalloop's env / kont arguments:
     doApp (Lam v bod) e2 st | isValue e2 = 
       maytrace ("    Doing App!  "++show (Lam v bod, e2)++" => "++show(subst e2 v bod))$
       evalloop (subst e2 v bod) kont st 
     doApp e1 e2 _ = error$ "Bad operator/rand in application: "++show e1++" "++show e2
     
     doPut :: Val d -> Val d -> RunState d -> Result d
     doPut (Varref l) (Q (QS set)) (Store amap, completed, runnable) =
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
                   kont (store', completed, runnable') void                     
         _ -> error$"put can only take a singleton query set, not: "++show set
     doPut e1 e2 _ = error$ "Bad put arguments: "++show e1++" "++show e2

     doGet :: Val d -> Val d -> RunState d -> Result d      
     doGet (Varref l) (Q (QS set)) (Store amap,completed,runnable) =
           case symMapLookup l amap of 
             Nothing -> error$"get: Unbound store location!: "++show l
             Just (d,waiting) -> 
                case checkReady d of
                  Just q -> kont state (singQ q) -- Victory!
                  Nothing ->      
                     -- It's not above us yet, capture continuation and place it in the store:
                     -- Continue processing another thread:
                     threadDispatch (Store$ symMapInsert l (d, putcont : waiting) amap,
                                     completed, runnable)
       -- Here we may need to add a new continuation to the waitlist,
       -- but that continuation is invoked whenever there is the
       -- possibility of a wakeup -- it's not a guarantee.
       where 
             -- Function invoked by put:
             putcont newd = case checkReady newd of 
                              Nothing -> Nothing
                              Just q  -> Just$ ThreadEffect (\ state -> kont state (singQ q))
               
             checkReady d =  -- Captures 'set':
               -- Find all the elements of our query set less than the current state:
               case filter (`joinLeq` d) (S.toList set) of
                []  -> Nothing
                [q] -> Just q
                ls  -> error$"Uniqueness criteria failed, compatible members of query set: "++show ls               

     doGet e1 e2 _ = error$ "Bad get arguments: "++show e1++" "++show e2


     doPrimApp :: Prim -> Val d -> Val d -> RunState d -> Result d      
     doPrimApp pr (Num a) (Num b) (Store amap,completed,runnable) = 
       case pr of
         Add  -> kont state (Num$ a+b)
         Mult -> kont state (Num$ a*b)         
     doPrimApp _ e1 e2 _ = error$ "Bad PrimApp arguments: "++show e1++" "++show e2
       
      -- Spawn threads to evaluate two expressions in parallel.
      -- Whenever both of those are completed, then reactivate the
      -- continuation (i.e. after the join).
     forkJoin continue e1 e2 = 
      -- Here we use raw exps as thread IDS, which is *INCREDIBLY* inefficient:
      let tid1 = e1; tid2 = e2
          -- Create two threads that don't sync on anything else,
          -- and a third thread that is waiting on the other two:
          t1 = ThreadValue tid1 e1
          t2 = ThreadValue tid2 e2 
          t3 = ThreadBlocked continue (tid1,tid2)
      in 
        -- We could jump directly to evaluating e1, but we go through the thread dispatch instead:
        threadDispatch (store, completed, t1 : t2 : t3 : runnable)
