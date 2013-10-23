{-# LANGUAGE Unsafe #-}

module Data.LVar.SLMap.Unsafe
       where

import qualified Data.Concurrent.SkipListMap as SLM

--  mp <- liftIO$ SLM.find (L.state lv) key

