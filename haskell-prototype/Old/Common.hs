

module Old.Common (forkWithExceptions) where

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
             Just E.ThreadKilled -> printf -- hPrintf stderr 
                                    "\nThreadKilled exception inside child thread, %s (not propagating!): %s\n" (show tid) (show descr)
	     _  -> do printf -- hPrintf stderr
                        "\nException inside child thread %s, %s: %s\n" (show descr) (show tid) (show e)
                      E.throwTo parent (e :: E.SomeException)
	 )

