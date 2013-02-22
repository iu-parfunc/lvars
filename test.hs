

import LVarTraceInternal
import Data.Set as S

main = do
  putStrLn$ "T0 "++show t0
  putStrLn$ "T1 "++show t1

--------------------------------------------------------------------------------

t0 :: Int 
t0 = runPar$ do i<-new; fork (return ()); put i 4; get i

t1 :: Int 
t1 = runPar$ do i<-new; fork (put i 4); get i


t2 :: IO (S.Set Int)
t2 = runParIO $
     do s <- newEmptySet
        mapM_ (\n -> fork$ putInSet n s) [1..10]
        waitForSetSize 10 s 
        consumeSet s


-- | Simple callback test.
t3 :: IO (S.Set Int)
t3 = runParIO $
     do s1 <- newEmptySet
        let fn e = putInSet (e*10) s1 
        s2 <- newEmptySetWithCallBack fn

        mapM_ (\n -> fork$ putInSet n s2) [1..10]
        waitForSetSize 10 s1
        consumeSet s1
