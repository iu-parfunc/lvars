
import System.Environment (getArgs)
import Control.LVish

main = do  
  [arg] <- getArgs
  let n = read arg
  
  putStrLn "AdjacencyGraph"
  print n
  print (n-1)
  
  for_ (0,n)   $ \ ix -> print ix
  for_ (0,n-1) $ \ ix -> print (ix+1)
    
  return ()
