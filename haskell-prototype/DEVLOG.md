

[2013.05.01] {Debugging test failures}

Lindsey and I just added a barrier for runPar to wait for its child threads.
This fixes some tests, but some still fail.

For example, i6a doesn't fail on one thread, but it does with multiple threads.
Here's an example of a failure:

    Setting the number of threads to: 4
    Main:
    Exception inside child thread "worker thread", ThreadId 9: Multiple puts to an IVar!
    Exception inside child thread "worker thread", ThreadId 12: Multiple puts to an IVar!
    Good.  Caught exception: Multiple puts to an IVar!
      i6a: [Failed]
    ERROR: Multiple puts to an IVar!

This is odd indeed!  It seems to be a failure to CATCH the exceptions.
Or rather, it looks like one gets caught and not the other?

Ah, yes the asynchronous exception mechanism could do that, right?  An
additional child thread throws an error after we've taken the first
error and exited the handler.


[2013.06.28] {New 1/500 failure on v3c}


    Main:
    Exception inside child thread "worker thread", ThreadId 12: Attempt to change a frozen LVar

    Exception inside child thread "worker thread", ThreadId 10: Attempt to change a frozen LVar
    Exception inside child thread "worker thread", ThreadId 11: Attempt to change a frozen LVar
    Caught allowed exception: Attempt to change a frozen LVar
      v3c: [Failed]
    ERROR: Attempt to change a frozen LVar

	     Test Cases  Total      
     Passed  0           0          
     Failed  1           1          
     Total   1           1          
