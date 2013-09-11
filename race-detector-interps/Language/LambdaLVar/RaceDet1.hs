{-# LANGUAGE TemplateHaskell, RankNTypes, CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

--------------------------------------------------------------------------------
-- VER 5: Evaluator that detects data races
--------------------------------------------------------------------------------

module Language.LambdaLVar.RaceDet1 where 

import qualified Language.LambdaLVar.UniqueDesugar as U

-- import Language.LambdaLVar.Common hiding (Exp(..), isValue, subst, get, put, void, singQ)
import Language.LambdaLVar.Common (QuerySet(..), Prim(..), Var, SymbolMap, isLocation, maytrace, var)
import qualified Language.LambdaLVar.Common as C
import qualified Data.Set as S
import Control.Monad.State.Strict (State, modify, runState, get, put)
import Control.Applicative ((<$>),(<*>))
import Algebra.Lattice
import Prelude as P hiding (fromInteger)
import Test.Framework.TH (testGroupGenerator)
import Test.HUnit ((@=?)) 
import Test.Framework.Providers.HUnit (testCase)

--------------------------------------------------------------------------------

-- Lambda-Par Expression parameterized over a domain:
data Exp d = 
  -- CHANGED forms.  The last argument is now a PEDIGREE value.
  -- This grammar assumes decimal-encoded pedigrees.
    Put (Exp d) (Exp d) (Exp d)
  | Get (Exp d) (Exp d) (Exp d)
  | Consume (Exp d)     (Exp d)
    
  -- Stay-the-same forms:
  | Varref Var
  | Num Integer
  | Q   (QuerySet d) 
  | Lam Var (Exp d)      
  | App (Exp d) (Exp d)
  | PrimApp Prim (Exp d) (Exp d)    
  | Reify  (Exp d)      
  | New
  | Unique  
 deriving (Show, Eq, Ord)
 

-- Rewrite already DESUGARED (UniqueDesugar.hs) expressions to pass the pedigree argument
-- directly into Put/Get/Consume.  This is relatively fragile.
rewrite :: (Show d) => C.Exp d -> Exp d
rewrite e = 
  case e of 
    -- Here's the pattern we expect:
    C.Lam p (C.Get   a b) -> Lam p (Get (rewrite a) (rewrite b) (Varref p))
    C.Lam p (C.Put   a b) -> Lam p (Put (rewrite a) (rewrite b) (Varref p))
    C.Lam p (C.Consume a) -> Lam p (Consume (rewrite a)         (Varref p))

    C.Get _ _   -> error$"rewrite: Get found in unexpected place: "++show e
    C.Put _ _   -> error$"rewrite: Put found in unexpected place: "++show e    
    C.Consume _ -> error$"rewrite: Consume found in unexpected place: "++show e
    C.Varref v      -> Varref v
    C.Num n         -> Num n
    C.Q q           -> Q q
    C.Lam v e       -> Lam v $ rewrite e 
    C.App a b       -> App (rewrite a) (rewrite b)
    C.Reify e       -> Reify (rewrite e)
    C.New           -> New
    C.Unique        -> Unique
    C.PrimApp p a b -> PrimApp p (rewrite a) (rewrite b)    

--------------------------------------------------------------------------------
-- Pedigrees
--------------------------------------------------------------------------------
    
-- | Here we use a more convenient encoding of pedigree (rather than Integer encodings).
--
-- LRJ = Left, Right, or [after the] Join:
data Branch = L | R | J  deriving (Eq, Show, Read)
type Pedigree = [Branch] 

fromString :: String -> Pedigree
fromString = map (read . (\x -> [x]))

-- Decode from an integer:
fromInteger :: Integer -> Pedigree
fromInteger num = loop num
 where 
   loop 0 = []
   loop n = 
    case quotRem n 10 of 
      (n',1) -> L : loop n'
      (n',2) -> R : loop n'
      (n',3) -> J : loop n'    
      _      -> error$"fromInteger: Bad integer encoding of pedigree path: "++show n

-- | isEarlier is a /partial/ order.  There may be no relationship at all.
isEarlier :: Pedigree -> Pedigree -> Maybe Ordering
isEarlier a b = helper (reverse a) (reverse b)
    where 
      helper [] [] = Just EQ
      helper [] _  = Just LT -- Shorter paths happen first.  (Will this happen?)
      helper _  [] = Just GT 
      -- If we're in a similar place, keep examining deeper in the path:
      helper (x:xs) (y:ys) | x == y = helper xs ys
      -- Either L/R branch happens EARLIER than the join point:
      helper _      (J:_) = Just LT
      helper (J:_)  _     = Just GT
      helper _      _     = Nothing

-- Additional synchronization edges are accumulated during evaluation.
-- These must be considered in ADDITION to the natural partial order on Pedigrees
data SyncEdge = BEFORE Pedigree Pedigree

-- A data race between a consume and another operation:
data DataRace = 
  DataRace { consumePed :: Pedigree, -- When did the consume happen
             otherPed   :: Pedigree, -- When did the other happen
             otherType  :: GetOrPut
           }
  deriving (Show, Eq)
           
data GetOrPut = GetOp | PutOp
  deriving (Show, Eq)

-- Logs PER-location.
type Logs d = SymbolMap [LogEntry d]

----------------------------------------------------------------------------------------------------

-- | The complete data-race detector.  This runs an input program and
--   returns a list of Consume operations that were UNDER-SYNCHRONIZED
--   (i.e. data races).b
detectConsumeRaces :: (Eq d, Show d, BoundedJoinSemiLattice d)
                   =>  C.Exp d -> (Exp d, Logs d, [(Var,DataRace)])
detectConsumeRaces origP = (e,logs, races)
 where 
   races = P.concatMap (\ (loc,logEs) -> P.map (\x -> (loc,x)) (raceTest logEs)) $ 
           C.symMapToList logs
   (e,logs) = runDesugNumPed  new
   new = rewrite $ U.desugar origP


-- | Gets and puts record the value that they put and return
--   respectively.  This enables post-mortem analysis of which get
--   unblocked which put.
data LogEntry d = LogPut     Pedigree d
                | LogGet     Pedigree d
                | LogConsume Pedigree
 deriving (Show, Eq)
          
isConsume (LogConsume _) = True
isConsume _              = False

getPed (LogPut p _)   = p
getPed (LogGet p _)   = p
getPed (LogConsume p) = p

-- | Is a given pattern of accesses a race?
raceTest :: forall d . (Eq d, JoinSemiLattice d) => [LogEntry d] -> [DataRace]
raceTest logs = 
    case P.filter isConsume logs of 
      [] -> [] -- No consumes means no consume data-races.
      [LogConsume ped] -> loop ped putGetsOnly
      oth -> error $ "raceTest: Multiple consumes on the same location.  This is ALWAYS an error."
  where 
    -- Here we look for any Consumes that are not fully synchronized
    -- by means of the stronger partial order (augmented with newEdges):
    loop _pedC [] = []
    loop pedC (hd : effects)
      | isStrictlyEarlier newEdges p pedC = []
      | otherwise                         = [DataRace pedC p PutOp]
     where p = getPed hd
    
    -- Additional ordering edges added by means of GETS:
    newEdges = collect putGetsOnly
    putGetsOnly = P.filter (not . isConsume) logs

    -- FIXME: this is a quick-and-dirty quadratic version.  It can easily be made log time.
    collect :: [LogEntry d] -> [SyncEdge]
    collect [] = []
    collect (hd:effects) = concatMap (fn hd) effects ++
                           collect effects
    fn (LogPut pp d1) snd = 
      case snd of 
        LogPut _ _   -> []     -- There's no such thing as a put-put datarace.
        LogGet pg d2 -> 
          if joinLeq d2 d1     -- Here the Put is a potential trigger for the get.
          then maybeEdge pp pg -- Thus pp must come strictly before pg.
          else []              -- Otherwise the put has nothing to do with the get.
    fn x y@(LogPut _ _)          = fn y x
    fn (LogGet _ _) (LogGet _ _) = [] -- No such thing as a get-get datarace.

    -- Assert that the first of two pedigrees SHOULD come first.  This may or may not add an additional edge.
    maybeEdge p1 p2 = 
      case isEarlier p1 p2 of
        Just LT -> []
        Just EQ -> error "raceTest: This should not happen"
        Just GT -> [] 
        Nothing -> [BEFORE p1 p2]  -- They are not related by the normal pedigree ordering, so we MAKE them related.
        

-- | Does one pedigree come strictly before another, given both the
--   intrisic partial order on pedigrees AND a set of additional sync edges.
isStrictlyEarlier :: [SyncEdge] -> Pedigree -> Pedigree -> Bool
isStrictlyEarlier extraEdges p1 p2 =
  case isEarlier p1 p2 of 
    Just LT -> True
    Just GT -> False
    Just EQ -> False
    Nothing -> search extraEdges 
  where  
    leq p1 p2 = 
      case isEarlier p1 p2 of 
        Just LT -> True
        Just EQ -> True
        _       -> False
    -- Check if each new edge might relate our two endpoints 
    -- (Surely there's a better algorithm for this!)
    search [] = False
    search (BEFORE a b : rest) | leq p1 a && leq b p2 = True
                               | otherwise            = search rest 

--------------------------------------------------------------------------------
-- Evaluator, copied and modified from Eval1


-- | This is the first step.  Evaluate the program to yield an answer,
--   and a complete trace of all put/get/consume effects.
--
--   Takes an expression and an interp function.
eval :: forall d . (Eq d, Show d, BoundedJoinSemiLattice d)
     => Exp d -> (S.Set d -> Exp d) -> (Exp d, Logs d)
eval eOrig interp = (final,logs)
 where
  (final,(_,logs)) = runState (outerloop eOrig) (C.symMapEmpty, C.symMapEmpty)
   
  outerloop :: Exp d -> State (SymbolMap d, Logs d) (Exp d)
  outerloop expr =     
    do (store,_) <- get       
       maytrace ("RACEDET1: "++ show store ++ "    " ++ show expr) $ do 
        expr' <- loop expr       
        if isValue expr'  
         then return expr' 
         else if expr == expr'
         then error$"No further evaluation possible, program stuck:\n "++show (expr)
         else outerloop expr'
          
  probation l = error$ "Probationary value for location "++show l++" touched!"

  -- This adds to the store and returns the OLD one.
  extendStore l d = do 
     (store,lgs) <- get
     put$ (C.symMapInsert l d store, lgs)
     return store

  getStore = fst <$> get -- Could use records here.

  logOp l entry = modify$ \ (st,lgs) -> 
    case C.symMapLookup l lgs of -- INEFFICIENT: if we expose a bigger API lookup/insert is unnecessary.
      Just ls -> (st, C.symMapInsert l (entry:ls) lgs)
      Nothing -> (st, C.symMapInsert l [entry] lgs)

  loop e =
    case e of 
      Q _      -> return e          -- Already a value.
      Lam _ _  -> return e          -- Already a value.
      Num _    -> return e          -- Already a value.
      Unique   -> return e -- This can't be reduced, and is NOT handled by this interpreter.
      
      Varref v | isLocation v -> return e -- Allowing as a value only for LOCATIONS.
      Varref v -> error$"unbound variable: " ++ show v

      -- No further evaluation of opera* is possible in this case.
      -- Only then perform beta reduction:
      App e1 e2 | isValue e1 && isValue e2 -> 
        case e1 of
          Lam v bod -> loop (subst e2 v bod)
          _ -> error$"Type error, non lambda in operator position: "++ show e1

      Reify (Q (QS s)) -> return (interp s)
      Reify (Q (QF _)) -> error "Should not call Reify on a predicate-based QuerySet"

      -- Global shared memory with global uniqueness.
      -- For now using Varrefs to represent labels:
      New -> do store <- getStore
                let fresh = var$ "l"++show (C.symMapSize store)
                extendStore fresh bottom
                return$ Varref fresh 

      Get (Varref l) (Q q) (Num ped) -> do
        store <- getStore
        case C.symMapLookup l store of
          Nothing -> error$"get: Unbound store location!: "++show l

          -- Here we find all the elements of our query set less than the current state:
          Just d  -> 
            let logit d = logOp l $ LogGet (fromInteger ped) d in
            case q of 
              QS set -> 
                case filter (`joinLeq` d) (S.toList set) of             
                 []  -> maytrace (" GET not ready : " ++ show set ++"  "++ show d) $
                        return e -- NO REDUCTION, return original.
                 -- NOTE: Logging only happens upon success:
                 [d] -> logit d >> return (singQ d)
                 ls  -> error$"Uniqueness criteria failed, compatible members of query set: "++show ls
              QF fn | Just d' <- fn d -> logit d' >> return (singQ d')
                    | otherwise       -> return e -- it does not satisfy the pred.

      Put (Varref l) (Q q) (Num ped) -> do
        store <- getStore
        let QS set = q in
          case S.toList set of 
            [d] -> let new = case C.symMapLookup l store of  
        		      Nothing -> d
        		      Just x  -> join d x
                   in do logOp l$ LogPut (fromInteger ped) d 
                         extendStore l new
                         return void
            _ -> error$"put can only take a singleton query set, not: "++show set
		    
      Consume (Varref l) (Num ped) -> do
          logOp l$ LogConsume (fromInteger ped) 
          -- Overwrite the existing entry in the store:
          store <- extendStore l (probation l) 
          case C.symMapLookup l store of       
            Nothing -> return (singQ bottom)
            Just x  -> return (singQ x)

      PrimApp pr v1 v2 | isValue v1 && isValue v2 ->
        case (v1,v2) of 
          (Num a, Num b) -> 
            case pr of Add  -> return (Num $ a + b)
                       Mult -> return (Num $ a * b)
          args -> error $"Bad value arguments to numeric primitive: "++show args

      -- Here we can't reduce the form, but we can let the children take a step:
      App e1 e2        -> App        <$> loop e1 <*> loop e2 
      Get e1 e2 e3     -> Get        <$> loop e1 <*> loop e2 <*> loop e3 
      Put e1 e2 e3     -> Put        <$> loop e1 <*> loop e2 <*> loop e3 
      Consume e1 e2    -> Consume    <$> loop e1 <*> loop e2 
      PrimApp pr e1 e2 -> PrimApp pr <$> loop e1 <*> loop e2       
      Reify   e1       -> Reify      <$> loop e1 
     




--------------------------------------------------------------------------------
-- Testing
--------------------------------------------------------------------------------

-- For quickcheck:

-- prop_1 a b = (isEarlier a b) == Just LT 
--     -- => (isEarlier b a) == Just GT

-- prop_2 a b = (isEarlier a b) == Just EQ 
--     -- => (isEarlier b a) == Just EQ
           
-- prop_3 a b = (isEarlier a b) == Nothing
--     -- => (isEarlier b a) == Nothing

-- prop_4 a =  (isEarlier [] a) == Just LT
--         || (isEarlier [] a) == Just EQ
        
--     -- (isEarlier b a) == Nothing           
           
-- Unit test cases:
fs = fromString
t0 = isEarlier (fs "LR") (fs "LLR")
t1 = isEarlier (fs "LLLLR") (fs "JR")



-- <Annoying duplication thanks to a slightly different datatype:>
--------------------------------------------------------------------------------
isValue :: Exp t -> Bool
isValue (Lam _ _)    = True
isValue (Q   _)      = True
isValue (Num   _)    = True
isValue (Varref v) | isLocation v = True
isValue _            = False

-- | By convention, this is our ignored/void value:
void :: Exp d
void = Q$QS S.empty

singQ :: d -> Exp d
singQ x = Q$ QS (S.singleton x)

-- | Basic let, non curried function application (sequential evaluation):
lett :: [(Var, Exp d)] -> Exp d -> Exp d
lett [] bod = bod
lett ((lhs,rhs):rest) bod = App (Lam lhs (lett rest bod)) rhs

-- A silly Reify function that produces strange unbound variables:
exampleReify :: Show d => S.Set d -> Exp d
exampleReify ls = Varref (var$ show ls)

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
  App e1 e2   -> App (loop e1) (loop e2)
  Put a b c   -> Put (loop a)  (loop b) (loop c)
  Get a b c   -> Get (loop a)  (loop b) (loop c)
  Consume x p -> Consume (loop x) (loop p)
  Reify  x -> Reify  (loop x)
  PrimApp p e1 e2 -> PrimApp p (loop e1) (loop e2)

-- Run a Unique-desugared program, inserting extra bindings.
runDesugNumPed :: (Eq d, Show d, BoundedJoinSemiLattice d) 
               =>  Exp d -> (Exp d, Logs d)
runDesugNumPed prog = eval 
      (lett [ (var "consL", cons 1)
            , (var "consR", cons 2)
            , (var "consJ", cons 3)]
       (App prog initP))
      exampleReify
 where 
   initP = Num 0
   cons n = Lam p $ PrimApp Add  (Num n)  $ 
                    PrimApp Mult (Num 10) $ 
                    Varref p

p = var "p"
--  </ End copy paste>
--------------------------------------------------------------------------------

-- Unit Tests:

l0 = var "l0"
case_race1 = (singQ (C.Full 33),
              C.symMapFromList [(l0,[LogGet [J,J] (C.Full 33),
                                     LogPut [R,J] (C.Full 33)])],
              [])
             @=? detectConsumeRaces U.p3a  
case_race2 = (singQ (C.Full 33),
              C.symMapFromList [(l0,[LogGet [R,J]   (C.Full 33),
                                     LogPut [R,L,J] (C.Full 33)])],
              [])
             @=? detectConsumeRaces U.p3b

-- Here's one with a race!
case_race3 = (singQ C.Empty,
              C.symMapFromList [(l0,[LogPut     [R,L,J] (C.Full 33),
                                     LogConsume [R,J]])],
              [(l0,DataRace {consumePed = [R,J], otherPed = [R,L,J], otherType = PutOp})])
  @=? detectConsumeRaces U.p5a
--(Q QS fromList [Full 33],[(l0,)],[])             
case_race4 = (singQ (C.Full 33),              
              C.symMapFromList [(l0,[LogConsume [J,J],
                                     LogPut [R,J] (C.Full 33)])],
              []) -- And then the race is fixed:
  @=? detectConsumeRaces U.p5b


-- Aggregate the unit tests above in a generated data structure:
raceTests = $(testGroupGenerator)

quicktest = do 
  print $ detectConsumeRaces U.p3a  
  print $ detectConsumeRaces U.p3b
  print $ detectConsumeRaces U.p5a
  print $ detectConsumeRaces U.p5b
