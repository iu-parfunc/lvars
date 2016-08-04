
import Control.Exception as E
import GHC.Conc
import qualified System.Posix.Signals as S
import System.Exit
import Text.Printf

t1 = do
  putStrLn "Thread 1 starting"
  t1id <- myThreadId
  -- forkIO (t2 t1id)
  forkWithExceptions forkIO "t2" (t2 t1id)
  putStrLn "Thread 1: done fork, now delay"
  threadDelay $ 100 * 1000
  putStrLn "Thread 1 ending.."

t2 t1id = do
  st <- threadStatus t1id
  putStrLn $"Thread 2 starting; t1 stat: "++show st
  threadDelay $ 1 * 1000 * 1000
  st <- threadStatus t1id  
  putStrLn $"Thread 2 waking to kill: t1 stat: "++show st
  throwTo t1id (ErrorCall "DIE t1!")
  st <- threadStatus t1id    
  putStrLn$ "Thread 2 -- throwTo returned, exiting... final t1 stat: "++show st
  -- exitFailure -- This just throws an exception, nothing more.

    -- Here's a more aggressive way to kill the process:
  S.raiseSignal S.sigABRT
  error "T2 DIE too..."

main = do
  mainid <- myThreadId 
  putStrLn$ "Main thread ID: "++show mainid
  -- forkIO t1
  forkWithExceptions forkIO "t1" t1 
  
  threadDelay $ 2000 * 1000
  putStrLn$ "Main thread exiting..."


-- | Exceptions that walk up the fork-tree of threads.
--   
--   WARNING: By holding onto the ThreadId we keep the parent thread from being
--   garbage collected (at least as of GHC 7.6).  This means that even if it was
--   complete, it will still be hanging around to accept the exception below.
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
               printf "\nThreadKilled exception inside child thread, %s (not propagating!): %s\n" (show tid) (show descr)
               return ()
	     _  -> do
                      printf "\nException inside child thread %s, %s: %s\n" (show descr) (show tid) (show e)
                      E.throwTo parent (e :: E.SomeException)
	 )


