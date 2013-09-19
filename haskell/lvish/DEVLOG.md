

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
---------------------------------------

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

[2013.06.28] {v2a failing as well}
----------------------------------

Context: I'm trying to add extra "catch"s to get all the exceptions
that might leak from child worker threads (using forkWithExceptions
rather than Control.Async).

Yesterday I saw thread-blocked-indefinitely errors from v2a, but today
I was having trouble reproducing that.  Yet if I pump the number of
threads up enough, I see the following:

    Exception inside child thread "worker thread", ThreadId 16: Attempt to change a frozen LVar
    Killing off workers.. [ThreadId 20,ThreadId 19,ThreadId 18,ThreadId 17,ThreadId 16,ThreadId 15,ThreadId 14,ThreadId 13]

    ThreadKilled exception inside child thread, ThreadId 20 (not propagating!): "worker thread"
    ThreadKilled exception inside child thread, ThreadId 19 (not propagating!): "worker thread"

    ThreadKilled exception inside child thread, ThreadId 18 (not propagating!): "worker thread"
    ThreadKilled exception inside child thread, ThreadId 17 (not propagating!): "worker thread"
    ThreadKilled exception inside child thread, ThreadId 14 (not propagating!): "worker thread"
    ThreadKilled exception inside child thread, ThreadId 13 (not propagating!): "worker thread"
      v2a: [Failed]ception inside child thread, ThreadId 15 (not propagating!): "worker thread"
    ERROR: EXCEPTION in runPar: Attempt to change a frozen LVar

(Yet, even then, I can only reproduce on a linux desktop, not on this macbook air.)



[2013.06.30] {Notes on PBBS and file IO}
----------------------------------------

PBBS has some very nice parallel parsing of large text files.

A quick test shows that on my laptop (with SSD), loading the following
adjacency list file....

    graphData/data/3Dgrid_J_10000000
    
Takes 6.34 seconds (built with gcc).  On a 3.1GHz Westmere
(marble.soic) it takes 4.97 seconds to read the file from /tmp/ one
one core after warming up.

But then with four threads (ICC/Cilk), it gets it done in 1.94
seconds.  

That's a file with 69,568,628 lines each containing a number that
needs to be parsed.  They use atol.


[2013.07.05] {Just observed an snzi4 failure}

Maybe this is known, but just in case it hasn't been seen before, here:

     snzi4: [Failed]
     ERROR: thread blocked indefinitely in an MVar operation

	      Test Cases   Total
      Passed  16           16
      Failed  1            1


It seems like I have to run it on the order of 75 times to reproduce
it, but it is reproducable.


[2013.07.07] {Aaron fixed forkInPool bug, but...}

I'm seeing failures on v3d like this:

     [!] Responding to env Var: DEBUG=5
     [dbg-lvish] About to fork workers...
      [dbg-lvish] Created new pool, pool identity= 19/20 transient cnt 0
     [dbg-lvish] Created new pool, pool identity= 21/22 transient cnt 0
     [dbg-lvish] Begin quiescing pool, identity= , pool identity= 21/22 transient cnt 1
     [dbg-lvish] -> Not quiescent yet, back to sched, pool identity= 21/22 transient cnt 1
     [dbg-lvish] forkInPool, pool identity= 19/20 transient cnt 2
      [Invocation 2] waiting on Just 3
     [dbg-lvish] forkInPool, pool identity= 19/20 transient cnt 3
      [Invocation 3] has no dependencies, running...
     [dbg-lvish] forkInPool, pool identity= 19/20 transient cnt 3
      [Invocation 1] waiting on Just 2
      [Invocation 2] dependency satisfied!
     [dbg-lvish] forkInPool, pool identity= 19/20 transient cnt 4
      [Invocation 4] waiting on Just 3
      [Invocation 4] dependency satisfied!
      [Invocation 1] dependency satisfied!
      [Invocation 2] dependency satisfied!
     [dbg-lvish] -> Quiescent now.. waking conts, pool identity= 19/20 transient cnt 1
     [dbg-lvish] forkInPool, pool identity= 19/20 transient cnt 1
      [Invocation 5] waiting on Just 4
      [Invocation 5] dependency satisfied!
     [dbg-lvish] -> Quiescent now.. waking conts, pool identity= 19/20 transient cnt 0
     [dbg-lvish] -> Quiescent now.. waking conts, pool identity= 21/22 transient cnt 0
     [dbg-lvish] Begin quiescing pool, identity= , pool identity= 19/20 transient cnt -1
     [dbg-lvish] -> Not quiescent yet, back to sched, pool identity= 19/20 transient cnt -1

It fails by getting stuck, it fails with:

   ERROR: EXCEPTION in runPar(ThreadId 5): Attempt to change a frozen LVar

And I even went back before Aaron's most recent bugfix and still got:

   ERROR: EXCEPTION in runPar(ThreadId 6): thread blocked indefinitely in an MVar operation

It is particularly unusual that the counter goes negative.

