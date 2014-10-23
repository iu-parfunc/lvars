{-# LANGUAGE RankNTypes #-}

import Control.LVish
import Control.Monad (forM_)
import Criterion.Main
import Criterion.Types


benchPar :: (forall s . Par e s a) -> Benchmarkable
benchPar par = Benchmarkable $ \ iters -> 
  runParPolyIO $ 
  do forM_ [1..iters] $ \_ -> do
       _ <- par
       return ()

fib m | m < 0     = error "negative!"
      | otherwise = go m
  where go 0 = 0
        go 1 = 1
        go n = go (n-1) + go (n-2)

main = defaultMain [
  bgroup "fib" [ bench "1"  $ whnf fib 1
               , bench "foo" $ benchPar (return ())
               ]
  ]

-- main = putStrLn "Hello world"

