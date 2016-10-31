{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE Rank2Types #-}

module Main where

import           Control.DeepSeq
import           Control.Monad.Trans
import           Data.Int
import           Data.Word
import           System.Directory
import           System.Environment
import           System.IO.Unsafe
import qualified System.Random.PCG.Fast.Pure as PCG

import           Control.LVish
import           Control.LVish.DeepFrz
import           Control.Par.EffectSigs
import qualified Data.LVar.PureSet      as PS

import           Graphics.Gnuplot.Advanced
import qualified Graphics.Gnuplot.File                 as File
import qualified Graphics.Gnuplot.Frame                as Frame
import qualified Graphics.Gnuplot.Frame.OptionSet      as Opts
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import qualified Graphics.Gnuplot.LineSpecification    as LineSpec
import qualified Graphics.Gnuplot.Plot.TwoDimensional  as Plot2D
import qualified Graphics.Gnuplot.Terminal.SVG         as SVG

import           Utils (Measured (..))
import qualified Utils as U

type Bench = IO [(Int64, Measured)]

type Det = Ef P G NF B NI
type Full = Ef P G F B I

{-# NOINLINE fromSize #-}
fromSize :: Int64
fromSize = unsafePerformIO $ read <$> getEnv "FROMSIZE"

{-# NOINLINE toSize #-}
toSize :: Int64
toSize = unsafePerformIO $ read <$> getEnv "TOSIZE"

{-# NOINLINE incr #-}
incr :: Int64
incr = unsafePerformIO $ read <$> getEnv "INCR"

{-# NOINLINE iters #-}
iters :: Int64
iters = unsafePerformIO $ read <$> getEnv "ITERS"

{-# INLINE benchPar #-}
benchPar :: (forall s . Par Full s ()) -> IO ()
benchPar p = U.for_ 1 iters (\_ -> runParNonDet p)

{-# INLINE measure #-}
measure :: NFData a => IO a -> IO Measured
measure f = do
  m <- U.measure iters f
  return (U.rescale m)

pureSetBench :: Bench
pureSetBench =
  U.fori' fromSize toSize incr $ \n -> measure $
    benchPar (PS.newEmptySet >>= \ps -> PS.insert (42 :: Int) ps)

main :: IO ()
main = print "TODO"
