module Data.LVar.FiltSet
       where

import Control.LVish

class SaturatingLVar f where
  -- | Drive the variable to top.  This is equivalent to an insert of a
  -- conflicting binding.
  saturate :: f a -> Par d s ()
  -- | Register a callback that is only called if the SatMap LVar
  -- becomes /saturated/.
  whenSat :: f a -> Par d s () -> Par d s ()
