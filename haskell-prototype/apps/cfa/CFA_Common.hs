{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}

module CFA_Common where

import Control.Applicative (liftA2, liftA3)
import qualified Control.Monad.State as State
-- import Control.Monad
-- import Control.Exception (evaluate)
-- import Control.Concurrent
-- import           System.IO.Unsafe (unsafePerformIO)
-- import           System.Mem.StableName (makeStableName, hashStableName)

-- import qualified Data.Map as M
-- import qualified Data.Set as S
-- import Data.List ((\\))
-- import Debug.Trace

-- import Control.LVish
-- import Control.LVish.Internal (liftIO)
-- import  Data.LVar.Set as IS
-- import  Data.LVar.Map as IM
-- import Text.PrettyPrint as PP
import Text.PrettyPrint.GenericPretty (Out(doc,docPrec), Generic)


--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

type Var = String
type Label = Int
data Exp = Halt | Ref Var | Lam Label [Var] Call deriving (Eq, Ord, Show, Generic)
data Call = Call Label Exp [Exp]                 deriving (Eq, Ord, Show, Generic)


instance Out Call
instance Out Exp

-- Helper functions for constructing syntax trees
-------------------------------------------------

type UniqM = State.State Int

newLabel :: UniqM Int
newLabel = State.state (\i -> (i, i + 1))

runUniqM :: UniqM a -> a
runUniqM = fst . flip State.runState 0

ref :: Var -> UniqM Exp
ref = return . Ref

lam :: [Var] -> UniqM Call -> UniqM Exp
lam xs c = liftA2 (flip Lam xs) newLabel c

call :: UniqM Exp -> [UniqM Exp] -> UniqM Call
call e es = liftA3 Call newLabel e (sequence es)

let_ :: Var -> UniqM Exp -> UniqM Call -> UniqM Call
let_ x e c = call (lam [x] c) [e]

halt :: UniqM Exp -> UniqM Call
halt e = call (return Halt) [e]

--------------------------------------------------------------------------------
-- Input Programs for Analysis
--------------------------------------------------------------------------------

-- The Standard Example
--
-- In direct style:
--
-- let id = \x -> x
--     a = id (\z -> halt z)
--     b = id (\y -> halt y)
-- in halt b
standardExample :: UniqM Call
standardExample = 
  let_ "id" (lam ["x", "k"] (call (ref "k") [ref "x"])) $
  call (ref "id") [lam ["z"] (halt (ref "z")),
                   lam ["a"] (call (ref "id") [lam ["y"] (halt (ref "y")),
                                               lam ["b"] (halt (ref "b"))])]

-- Example with free varibles (showing escapes):
fvExample :: UniqM Call
fvExample = 
  let_ "id" (lam ["x", "k"] (call (ref "k") [ref "x"])) $
  call (ref "id") [lam ["z"] (call (ref "escape") [ref "z"]),
                   lam ["a"] (call (ref "id") [lam ["y"] (call (ref "escape") [ref "y"]),
                                               lam ["b"] (call (ref "escape") [ref "b"])])]

-- Look here for more:
-- https://github.com/ilyasergey/reachability/tree/master/benchmarks/gcfa2

#if 0
blur :: UniqM Call
blur =
  let_ "id" (lam ["x", "k"] (call (ref "k") [ref "x"])) $
  let_ "blur" (lam ["y", "k"] (call (ref "k") [ref "y"])) $
  let_ "lp" (lam ["a", "n", "k"]
             -- if
             (call (ref "k") [ref "y"])) $  
---------------------------------------------------------
-- (letrec ((id (lambda (x) x))
--          (blur (lambda (y) y))
--          (lp (lambda (a n)
--                (if (<= n 1)
--                    (id a)
--                    (let* ((r ((blur id) #t))
--                           (s ((blur id) #f)))
--                      (not ((blur lp) s (- n 1))))))))
--   (lp #f 2))
#endif


