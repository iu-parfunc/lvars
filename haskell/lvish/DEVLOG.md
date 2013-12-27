

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


[2013.10.24] {Making progress in debugging}
-------------------------------------------

Just fixed Pure/MaxCounter.  Starting to try to isolate the unit-test
flakiness.

Right now, all 48 tests pass for me most of the time.  If I try to run
the full thing 50 times, I can see a deadlock (when running tests in
parallel).

Interestingly test-framework IS getting a substantial parallel speedup
if I do NOT pass -j1.  If I pass -j1 I am NOT able to reproduce the
deadlock currently (at -N1).

When the tests are running in parallel, it does seem that the wrong
test gets the exception, for example, this:

      i3c: [Failed]
    ERROR: Got the wrong exception, expected one of the strings: ["Attempt to change a frozen LVar"]
    Instead got this exception:
      "ConflictingPutExn \"Multiple puts to an IVar! (obj 20 was 19)\""

It can now pass many dozens of complete tests (48 tests) like this:

    rep 30 ./dist/build/test-lvish/test-lvish -j1 +RTS -N1
    
And with -N4 it actually passes as well!  (Keep in mind, however, that
my current build has `-fdebug` enabled at compile time, even if I am
not setting the DEBUG env var! )

So where'd the problems go?
Are they ONLY appearing when we remove `-j1`?  In fact, I'm also
having trouble reproducing the above mismatched error problems even
with NO `-j1` but also `+RTS -N4` added.  (Is the best reproducer
something like `-j8 +RTS -N1`?  No, now I'm having trouble reproducing
the i3c failure from a moment ago under any conditions...)

Indeed... debugging assertions seem to have something to do with it.
If I recompile with `-f-debug -f-chaselev`, then I quite quickly (9
iterations) produce a deadlock when running tests in parallel.
Subsequent tries took longer... but `+RTS -N4` seems to help and it is
definitely possible to consistently get these deadlocks.

That's something!  Let's toggle back to debug one more time to double
check... `-fdebug -f-chaselev`.  Now I can do 50 full reps with
`-N4`... but wait, here's an error:

      v3e: [Failed]
    ERROR: LVarSpecificExn "EXCEPTION in runPar(ThreadId 5): PutAfterFreezeExn \"Attempt to change a frozen LVar\""


Or sometimes it pops up elsewhere, say v2a.  I must be careful here because
setting `+RTS -N1` behaves very differently from setting no thread setting at
all.  The reason is that we are passing `-N4` by default in the cabal settings:

     -O2 -threaded -rtsopts -with-rtsopts=-N4

And this setting for numCapabilities seems to effect the
test-framework driver as well.


[2013.12.02] {Debugging pmapFold for SLMap}
-------------------------------------------


[2013.12.11] {Debugging issue with testing framework}
------------------------------------------------------------

It was appearing as spurious thread blocked failures:

       Common:
	 SLMapTests:
	   v7a: [OK]
	   v8c: [OK]
	   v8d: [OK]
      [lvish-tests] Test timed out -- thread blocked!
	   v9a: [Failed]
     assertNoTimeOut: timeout occurred after 1.0 seconds
	   handlrDup: [OK]
	      Test Cases  Total
      Passed  5           5
      Failed  1           1
      Total   6           6
     real	0m0.009s
     user	0m0.004s
     sys	0m0.003s

This was a bug.  Misinterpreting ThreadBlocked, fixed now.

[2013.12.12] {More testing woes}
------------------------------------------------------------

We appear to be consistently locking up on test v9e atm.
Oh wait.. is it just taking an excessive amount of time?

No, sometimes its getting past that test and going a couple tests
further, possibly getting stuck on v9f.  I hope test-framework is
flushing stdout and giving us an accurate representation of how many
tests we've gotten through...

Test-framework *does* have systematic timeouts.  They just don't
actually work.  It still gets stuck.  Though they seem to work better
if -j1 is provided.

    time ./Main.exe -j1 --timeout=1 +RTS -N4
    
These stuck tests do seem to be deadlock rather than livelock --
they're not burning CPU.  With the above single-threaded timeout
approach we can get through all the tests, but a nondeterministic
number of them pass.

I'm seeing 7-10 failures out of 57 currently.  That's with -N4.  With
-N1 everything works fine.

As mentioned in the discussion of issue #11, some of these failures
manifest not as timeouts but as indefinite-blockage:

    ERROR: LVarSpecificExn "EXCEPTION in runPar(ThreadId 5): thread blocked indefinitely in an MVar operation"

E.g. for v3d.  

Note that this pattern of test failures existed before the change to
addHandler...  Actually, the test failures seem to have been worse
before.  It would deadlock even with "-j1 --timeout=3".

Debugging v9e:
-----------------------------

This is actually a pretty nice situation, because it will fail even
with one test enabled, and on only two threads:

    cd lvish/tests
    time ./Main.exe -tv9e -j1 --timeout=1 +RTS -N2

This is fine too:

    DEBUG=3 time ./ArrayTests.exe -tv9e -j1 --timeout=1 +RTS -N2

Looking for a different test to debug...
----------------------------------------

Are there any that fail but don't use istructure or map variants?
    
Ok, as of rev 88d540d (depth 898), we COULD pass the basic tests, or
at least 48 of them, on four threads.  Namely, these tests.

     :LVishAndIVarv0
     :LVishAndIVarv1a
     :LVishAndIVarv1b
     :LVishAndIVari3f
     :LVishAndIVari3g
     :LVishAndIVarlp01
     :LVishAndIVarlp02
     :LVishAndIVarlp03
     :LVishAndIVarlp04
     :LVishAndIVardftest0
     :LVishAndIVardftest1
     :LVishAndIVardftest3
     :LVishAndIVarshow01
     :MemoTests02seq
     :MemoTests03seq
     :MemoTests04seq
     :LogicalTestsand1
     :LogicalTestsand2
     :LogicalTestsand3
     :LogicalTestsand4
     :LogicalTestsor1
     :LogicalTestsor2
     :LogicalTestsor3
     :LogicalTestsor4
     :LogicalTestsandMap01
     :LogicalTestsorMap01
     :MapTestsv7a
     :MapTestsi7b
     :MapTestsv7c
     :MapTestsv8c
     :MapTestsv8d
     :MapTestsshow02
     :MapTestsshow03
     :SetTestsv2a
     :SetTestsv2b
     :SetTestsv2c
     :SetTestsv3b
     :SetTestsi3c
     :SetTestsv3d
     :SetTestsv3e
     :SetTestsv8a
     :SetTestsv8b
     :SetTestsshow05
     :SetTestsshow06
     :SetTestsshow05B
     :SetTestsshow06B
     :MaxCounterTestsmc1
     :MaxCounterTestsmc2

But now we're having failures on 8 tests.  Even when we disable the
new generic tests (-f-generic).  Here are some of the failures:

 * v9e - natarray
 * v9g - istruct
 * v9f - ivar array
 * v8d - map test: traverse, union, foreach
   (traverse property for maps also fails)

 * i3c - sets: undersynchronized
 * v3e - sets: foreach, waitElem
 * v8a - sets: cartesian product
 * v8b - sets: 3-way cartesian

Ok, from all this it looks like there might be a general failure in
callbacks/addHandler.  Or perhaps a general failure in handler pools.

v9f is interesting however...  It uses no callbacks or handler pools.
It simply writes N ivars and then reads them.


[2013.12.26] {Adding a schedule-control facility to the debug-logging one}

The first draft is now running... but it's chewing up a bunch of user
time.  A tiny 0.3 second test goes to 2 seconds, and then if I turn on
more print messages (on every time around "schedloop"), I get this:

      100.12 real        97.08 user         3.01 sys

Ok, there is some busy-waiting in there.  One weird thing is that ONE
test runs quickly.  Oh, I see.  The logger threads aren't getting
killed so they continue to spin after things are shut down.

I fixed that problem... but when printing out each schedloop
invocation it still goes crazy slowly (21.8s user).  It should be able
to spam messages to the terminal a heck of a lot faster than that.
We're talking slow enough for me to read it as it scrolls by. 

With that print disabled, it runs in the SAME time whether we use
"yield" or "threadDelay" for 10ms...


