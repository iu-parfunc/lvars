{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module CFA_Common where

import Control.DeepSeq
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

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (Test(..))

-- k-CFA parameters

k_param :: Int
k_param = 3

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

type Var = String
type Label = Int
data Exp = Halt | Ref Var | Lam Label [Var] Call deriving (Eq, Ord, Show, Generic)
data Call = Call Label Exp [Exp]                 deriving (Eq, Ord, Show, Generic)


instance Out Call
instance Out Exp

instance NFData Exp where
  rnf Halt = ()
  rnf (Ref v) = rnf v
  rnf (Lam !l ls call) = seq (rnf ls) (rnf call)

instance NFData Call where
  rnf (Call !l e1 ls) = seq (rnf e1) (rnf ls)
  
-- Helper functions for constructing syntax trees
-------------------------------------------------

type UniqM = State.State Int

newLabel :: UniqM Int
newLabel = State.state (\i -> (i, i + 1))

newVar :: String -> UniqM String
newVar s = do n <- newLabel
              return (s ++ show n)

runUniqM :: UniqM a -> a
runUniqM = fst . flip State.runState 0

ref :: Var -> UniqM Exp
ref = return . Ref

lam :: [Var] -> UniqM Call -> UniqM Exp
lam xs c = liftA2 (flip Lam xs) newLabel c

call :: UniqM Exp -> [UniqM Exp] -> UniqM Call
call e es = liftA3 Call newLabel e (sequence es)

call' :: String -> [String] -> UniqM Call
call' e es = call (ref e) (map ref es)

let_ :: Var -> UniqM Exp -> UniqM Call -> UniqM Call
let_ x e c = call (lam [x] c) [e]

halt :: UniqM Exp -> UniqM Call
halt e = call (return Halt) [e]

-- true :: UniqM Exp
-- true = do a <- newVar "left"
--           b <- newVar "right"
--           k <- newVar "kont"
--           lam [a,b,k] $ (call (ref k) [ref a])

-- false :: UniqM Exp
-- false = do a <- newVar "left"
--            b <- newVar "right"
--            k <- newVar "kont"
--            lam [a,b,k] $ (call (ref k) [ref b])


true  = ref "true"
false = ref "false"
not_ ls = call (ref "not") ls

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

-- | Create repeated copies of an example to use up more time.
scaleExample :: Int -> UniqM Call-> UniqM Call
scaleExample 1 ex = ex
scaleExample n ex =
  let f s = s ++ show n in
  let_ "const" (lam [f "x", f "y", f "k"] (call (ref (f "k")) [ref (f "x")])) $ 
    call (ref "const") [lam [f "ignore1_"] (scaleExample (n-1) ex),
                        lam [f "ignore2_"] ex,
                        lam [f "z"] (halt (ref (f "z")))]

-- This is very quick for 1..3 but then diverges at 4!
standardBig :: Int -> UniqM Call
standardBig 0 = error "standardBig must take at least 1"
standardBig n =
  let_ "id" (lam ["x", "k"] (call (ref "k") [ref "x"])) $
  loop n (error "shouldn't happen")
  where
    loop 0 a = halt (ref a)
    loop n a = do 
      let z = "z"++show n
          a = "a"++show n
      call (ref "id") [lam [z] (halt (ref z)),
                       lam [a] (loop (n-1) a)]
        

  
-- Look here for more:
-- https://github.com/ilyasergey/reachability/tree/master/benchmarks/gcfa2

blur :: UniqM Call
blur =
  let_ "id"   (lam ["x","k1"]  (call' "k1" ["x"])) $
  let_ "blur" (lam ["y", "k2"] (call' "k2" ["y"])) $
  let_ "lp" (lam ["lp0","a", "n", "k0"]
             (call (ref "leq")
              [ref "a", ref "one",
               lam ["bool"]$
               call (ref "bool")
                 [ lam["k3"] (call' "id" ["a", "k3"])
                 , lam["k4"] body
                 , lam["eta"] (call' _HACK ["eta"])]
              ])) $ 
    (call (ref "lp")
     [ref "lp", false, ref "two",
      lam["fin"] (halt (ref "fin"))])
 where
   body = (call (ref "blur")
           [ref "id",  lam ["rr"] $ 
            call (ref "rr") [true, lam ["r"] $
            (call (ref "blur")
             [ref "id",  lam ["ss"] $ 
              call (ref "ss") [false, lam ["s"] $
               (call (ref "blur")
                [ref "lp0", lam ["anon"] $
                 call (ref "sub1") [ref "n", ref "one",
                  lam["n2"]$ 
                   call (ref "anon") [ref "s", ref "n2",
                    lam ["n3"] $
                     not_ [ref "n3", ref "k4"]
                     ]]])
                ]])
             ]])
--   _HACK = "k0" -- This is getting an error (unbound in store)
   _HACK = "k99" -- hack for now...
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




-- mj09
-- (let ((h (lambda (b)
-- 	   (let ((g (lambda (z) z)))
-- 	     (let ((f (lambda (k)
-- 			(if b
-- 			    (k 1)
-- 			    (k 2)))))
-- 	       (let ((y (f (lambda (x) x))))
-- 		 (g y)))))))
--   (let ((x (h #t)) (y (h #f)))
--     y))
----------------
-- CPS:


--------------------------------------------------------------------------------

makeMain :: (UniqM Call -> IO ()) -> IO ()
makeMain runExample = defaultMain$ hUnitTestToTests$ TestList
  [ TestLabel "fvExample"       $ TestCase (runExample fvExample)
  , TestLabel "standardExample" $ TestCase (runExample standardExample)
  , TestLabel "scale1" $ TestCase (runExample$ scaleExample 80 fvExample)
  , TestLabel "scale2" $ TestCase (runExample$ standardBig 4)
  , TestLabel "blur"   $ TestCase (runExample blur)
    
  , TestLabel "simple" $ TestCase $ runExample$
    let_ "fst" (lam ["x","y","k"] (call (ref "k") [ref "x"])) $ 
    let_ "f" (lam ["z"] (call (ref "z") [ref "z"])) $    
    call (ref "fst") [ref "f",
                      ref "fst", 
                      lam ["l"] (halt (ref "l"))]
    
  , TestLabel "omega" $ TestCase $ runExample$
    let_ "f" (lam ["x"] (call (ref "x") [ref "x"])) $
    call (ref "f") [ref "f"]

  ]

