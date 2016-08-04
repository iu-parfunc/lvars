{-# LANGUAGE CPP #-}

module Common (forkWithExceptions) where

import Control.Exception as E
import Control.Concurrent hiding (yield)
import Text.Printf

-- | Exceptions that walk up the fork tree of threads:
forkWithExceptions :: (IO () -> IO ThreadId) -> String -> IO () -> IO ThreadId
forkWithExceptions forkit descr action = do 
   parent <- myThreadId
   forkit $ do
      tid <- myThreadId
      E.catch action
	 (\ e -> 
           case E.fromException e of 
             Just E.ThreadKilled -> do
-- Killing worker threads is normal now when exception handling, so this chatter is restricted to debug mode:
#ifdef DEBUG_LVAR               
               printf "\nThreadKilled exception inside child thread, %s (not propagating!): %s\n" (show tid) (show descr)
#endif
               return ()
	     _  -> do
#ifdef DEBUG_LVAR               
                      printf "\nException inside child thread %s, %s: %s\n" (show descr) (show tid) (show e)
#endif
                      E.throwTo parent (e :: E.SomeException)
	 )

