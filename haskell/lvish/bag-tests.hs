
import Prelude hiding (write, read, freeze, replicate)
import Data.Vector.Mutable
import Data.Vector (freeze)
import Data.Maybe

import Data.Atomics
import Data.Atomics.Internal

import qualified Data.Concurrent.SBag as B

main = do
  test0
  test1
  test2
  test3
  test4
  test5
  test6
  test7
  test8

test0 = do
  bag <- B.new
  return ()
      
test1 = do
  bag <- B.new
  B.initThread bag
            
test2 = do    
  bag <- B.new
  B.initThread bag
  B.insert bag 120
        
test3 = do
  bag <- B.new
  B.initThread bag
  B.insert bag 120
  res <- B.tryGetAny bag
  putStrLn $ show $ fromJust res
    
test4 = do
  bag <- B.new
  B.initThread bag
  B.insert bag 1
  B.insert bag 2
  res1 <- B.tryGetAny bag
  res2 <- B.tryGetAny bag
  putStrLn $ (show $ fromJust res1) ++ " " ++ (show $ fromJust res2)
                                                               
test5 = do
  bag <- B.new :: IO (B.SBag Int)
  B.initThread bag
  mapM_ (B.insert bag) [0..6]
  mapM_ (\n -> do
            v <- B.tryGetAny bag
            putStrLn $ show v)
        [0..6]

test6 = do
  bag <- B.new :: IO (B.SBag Int)
  mapM_ (B.insert bag) [0..2]
  mapM_ (\n -> do
            v <- B.tryGetAny bag
            putStrLn $ show v)
         [0..2]
  mapM_ (B.insert bag) [3..5]
  mapM_ (\n -> do
            v <- B.tryGetAny bag
            putStrLn $ show v)
        [0..2]
    
test7 = do
  bag <- B.new :: IO (B.SBag Int)
  mapM_(B.insert bag) [0..2]
  mapM_ (\n -> do
            v <- B.tryGetAny bag
            putStrLn $ show v)
        [0..4]
  mapM_(B.insert bag) [3..5]
  mapM_ (\n -> do
             v <- B.tryGetAny bag
             putStrLn $ show v)
        [0..4]

test8 = do
  arr <- replicate 5 False
  x <- B.readArrayForCas (arr :: IOVector Bool) 2
  xValue <- peekTicket (x :: Ticket Bool)
  (bool, ticket) <- B.casIOArray arr 2 x True
  res <- peekTicket (ticket :: Ticket Bool)
  putStrLn (show (xValue == False) ++ " " ++ show (res == True))
  
  