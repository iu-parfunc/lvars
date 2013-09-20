
import Prelude hiding (write, read, freeze, replicate)
import Data.Vector.Mutable
import Data.Vector (freeze)

one :: IO String
one = do 
  arr <- replicate 2 (5 :: Int)
  write arr 1 120
  fa <- freeze arr
  return $ show fa
  
data Foo = Bar { n :: Int }

two = nn
  where
    bar = Bar 120
    nn = n bar

three :: IO Int
three = do
  arr <- replicate 2 (5 :: Int)
  read arr 1

data Baz a = Baz (IOVector (Maybe a))

four :: Maybe Int -> Int
four x = case x of
  Just n -> n
  Nothing -> 120
    
              
          
          