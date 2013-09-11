
module Tests where

import qualified Data.Set as S

import Language.LambdaLVar.Common
import Language.LambdaLVar.Eval1 (eval)
import qualified Language.LambdaLVar.Eval2 as E2
-- import qualified Language.LambdaLVar.Eval3 as E3
import qualified Language.LambdaLVar.Eval4 as E4

import System.IO (stderr, hPutStrLn)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (forM_)

--------------------------------------------------------------------------------
-- Examples and Test programs:
--------------------------------------------------------------------------------

x = var "x"
y = var "y"
z = var "z"

----------------------------------------
p0 :: Exp (IVar Integer)
p0 = App (App (Lam x (Lam y (Varref x))) 
	  (Put New (Q$QS (S.singleton$ Full 100))))
          (Put New (Q$QS (S.singleton$ Full 200)))

t0 :: (Exp (IVar Integer), SymbolMap (IVar Integer))
t0  = eval    p0 exampleReify
-- t0b = evalCPS p0 exampleReify
t0c = E2.evalThreaded p0 exampleReify

----------------------------------------
p1 :: Exp (IVar Integer)
p1 = letpar [(x, (Q$QS (S.singleton$ Full 100))),
	     (y, (Q$QS (S.singleton$ Full 200)))]
      (singQ Empty)

t1 = eval p1 exampleReify
t1c = E2.evalThreaded p1 exampleReify

----------------------------------------
p2 :: Exp (IVar Integer)
p2 = lett   [(x, New)] $ 
     letpar [(y, Get (Varref x) (fullIvarQ [1..5])),
	     (z, Put (Varref x) (fullIvarQ [3]))]
      (Varref y)

t2 = eval p2 exampleReify
t2c = E2.evalThreaded p2 exampleReify

----------------------------------------
-- Multiple put error:
p3 :: Exp (IVar Integer)
p3 = lett   [(x, New)] $ 
     letpar [(y, Put (Varref x) (fullIvarQ [1])),
	     (z, Put (Varref x) (fullIvarQ [3]))]
      (Varref y)

t3 = eval p3 exampleReify

----------------------------------------
p4 :: Exp (IVar Integer)
p4 = App (Lam x (Varref x)) void

t4 = eval p4 exampleReify
t4c = E2.evalThreaded p4 exampleReify


----------------------------------------
-- Now for some arithmetic
p5 :: Exp ()
p5 = Num 3

--------------------------------------------------------------------------------
-- Testing Infrastructure:
--------------------------------------------------------------------------------

type Elt = Integer -- Fixed for now.

type Evaluator = Exp (IVar Elt) -> 
                 (S.Set (IVar Elt) -> Exp (IVar Elt)) -> 
                 (Exp (IVar Elt), SymbolMap (IVar Elt))

-- | All the evaluators provided by the various modules imported above:
allEvals :: [Evaluator]
allEvals = [eval
           , E2.evalThreaded
--           , E3.evalThreaded
           , E4.evalThreaded
           ]
           
-- Run one program through multiple evaluators and check that they get the same answer.
testOne :: Exp (IVar Elt) -> Bool 
testOne prog = unsafePerformIO$ do
    hPutStrLn stderr$ "\nPutting the following test program through "++show(length allEvals)++" different evaluators:"
    hPutStrLn stderr "================================================================================"
    hPutStrLn stderr ("   "++ show prog)
    hPutStrLn stderr "================================================================================"
    hPutStrLn stderr "First, our reference evaluator:"
    print evaled1  
    let doesUnify = map (unify evaled1) evaledRest
    forM_ (zip [2..] doesUnify) $ \ (ind,x) -> do
      hPutStrLn stderr$ "\nNext, checking evaluator # "++show ind++" against reference."
      hPutStrLn stderr "----------------------------------------"
      case x of 
        True  -> hPutStrLn stderr "Passed (same answer)."
        False -> hPutStrLn stderr "FAILED."            
    return$ all id doesUnify
  where 
    evaled1:evaledRest = map (\ eval -> eval prog exampleReify) allEvals

-- Check if two configurations (exp,store) are equivalent up to
-- renaming:
unify :: (Ord d) 
      => (Exp d, SymbolMap d) -> (Exp d, SymbolMap d) -> Bool
unify (e1,s1) (e2,s2) = 
  expsMatch &&
  -- TEMP -- for now just check if the same SET of values is in the
  -- store, ignoring all labels entirely:
  S.fromList (symMapElems s1) == 
  S.fromList (symMapElems s2)
 where 
  expsMatch =
   case (e1,e2) of
     (Varref l1, Varref l2) -> error "TODO: finish this case"
     (Lam v1 b1, Lam v2 b2) -> error "TODO: finish this case"
     _ -> e1 == e2
