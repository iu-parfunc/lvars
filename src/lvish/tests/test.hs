{-# LANGUAGE BangPatterns #-}
module Main where

import Control.LVish
import Data.LVar.PureMap
import Data.Time.Clock
import System.IO.Unsafe
import Control.Par.Class
import Control.Par.Class.Unsafe (internalLiftIO)

size :: Int
size = 10000000

main :: IO ()
main = do
  !start <- getCurrentTime
  ans <- runParNonDet $ isND $ do
    mp <- newFromList $ zip [1..size] [1..size]
    fmp <- freezeMap mp
    pmapFold (\(_ , v) -> return v)
             (\v1 v2 -> return $ v1 + v2)
             0 fmp
  !end <- getCurrentTime
  putStrLn $ show ans
  putStrLn $ (show ((realToFrac $ diffUTCTime end start) * 1000.0 ::Double)) ++ " ms"
  return ()

