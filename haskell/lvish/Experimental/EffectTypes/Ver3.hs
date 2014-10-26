{-# LANGUAGE DataKinds, KindSignatures, GADTs, TypeOperators, CPP,
    GeneralizedNewtypeDeriving, FlexibleInstances, TypeFamilies, RankNTypes,
    ConstraintKinds, FlexibleContexts, UndecidableInstances, ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Monad
import Control.Par.EffectSigs
-- import Control.Monad.Trans

class ParMonad (p :: EffectSig -> * -> * -> *) 
  where
  pbind :: p e s a -> (a -> p e s b) -> p e s b
  preturn :: a -> p e s a 

class ParMonadTrans (t :: (EffectSig -> * -> * -> *) -> 
                          (EffectSig -> * -> * -> *)) where
  plift :: ParMonad p => p e s a -> (t p) e s a

instance ParMonad p => Monad (p e s) where
  m >>= f = pbind m f 
  return = preturn 

{-
class Monad (p e s) => 
      ParMonad (p :: EffectSig -> * -> * -> *) 
               e s 
   where
-}
-- instance Monad (p e s) => ParMonad p where

-- foo :: (ParMonad p, Monad (p e s)) => Int -> p e s Int
foo :: (ParMonad p) => Int -> p e s Int
foo n = do _ <- undefined
           return n
