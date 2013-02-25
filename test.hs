

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


-- | Simple test of pairs.
t4 = runParIO $
     do p <- newPair
        putFst p 3
        putSnd p "hi"        
        x <- getFst p
        y <- getSnd p
        return (x,y)

-- This seems pretty naughty:        
t5 = runParIO $
     do p <- newPair
        putFst p 3
        putSnd p "hi"
        putSnd p "there"        
        getFst p

-- More pairs
t6 = runParIO $
     do p1 <- newPair
        p2 <- newPair
        fork$ do x <- getFst p1
                 putSnd p2 x 
        fork$ do x <- getSnd p2
                 putSnd p1 x
        putFst p1 33        
        getSnd p1

