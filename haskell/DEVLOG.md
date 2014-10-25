

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


[2013.12.16] {Working on fixing a scheduler bug}
--------------------------------------------------------------------------------

The bug is that the final global poll check on getLV doesn't obey the
GET_ONCE stricture.

This is in the context of the v9f test.  Alas... the fix that I tried
(simply doing the execFlag check), is leading to deadlock or
blocked-indefinitely-on-mvar exceptions.

It takes about NUMELEMS=3000 to trigger the the deadlock in a
reasonable number of iterations.

Interestingly not MANY of these iterations are blocking at all.  The
writing thread is getting enough ahead that usually no gets block at
all.

Actually... now, with my "fix", I'm seing duplication AND deadlock
with GET_ONCE.  Hmm.




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

-----------

Update: I just switched it to exponential backoff everywhere it
spins/polls.  I'm still seeing the same times.

Ah!  Interesting.  Now it runs much faster when running on multiple
real threads!  Maybe there are insufficient yield points to enable it
to run quickly (cooperatively) on one thread?  Or maybe the
differences in the threaded RTS matter here... (the threaded RTS
should be linked either way, but perhaps parts of it are inactive in
-N1).

Is there a path around schedloop that does NOT yield?  

Deterministically exposing the task-graph 
-----------------------------------------

Test v1a is currently demonstrating a problem with nondeterminism,
sometimes the infrastructure observes the parallelism, seeing these
tasks together:

    1:  [dbg-lvish] getLV: blocking on LVar, registering listeners, returning to sched...
    2:  [dbg-lvish] putLV: about to mutate
    
And sometimes it doesn't:

    -----
      1:  [dbg-lvish] Initializing Logger...
    -----
      1:  [dbg-lvish] putLV: about to mutate
    -----
      1:  [dbg-lvish] getLV: blocking on LVar, registering listeners, returning to sched...
    -----

Actually on one thread it always fails to observe the
parallelism... On -N2 or -N4 it sometimes does.  In fact, on one
thread it doesn't observe the getLV in the logger at all!

`

[2013.12.30] {Debugging issue #57}
-----------------------------------

I'm dropping in more logging messages to try to find what the
data-race is.  One thing I wasn't expecting is that there are TWO
getLV threshold checks racing with eachother in parallel:

    8| #3 of 4:  [dbg-lvish] putLV: setStatus 19 on worker 3
    8| #2 of 3:  [dbg-lvish] putLV: setStatus 19 on worker 1
    7| #1 of 3:  [dbg-lvish] getLV: first readIORef 19 on worker 2
    5| #3 of 4:  [dbg-lvish] putLV: about to mutate lvar 19 on worker 3
    7| #1 of 3:  [dbg-lvish] getLV (active): check globalThresh 19 on worker 2
    5| #1 of 3:  [dbg-lvish] putLV: about to mutate lvar 19 on worker 1
    8| #3 of 3:  [dbg-lvish] putLV: setStatus 19 on worker 4
    4| #1 of 3:  [dbg-lvish] getLV: blocking on LVar, registering listeners 19 on worker 2
    8| #3 of 3:  [dbg-lvish] putLV: read final status before unsetting19 on worker 3
    8| #3 of 3:  [dbg-lvish] putLV: read final status before unsetting19 on worker 1
    5| #3 of 3:  [dbg-lvish] putLV: about to mutate lvar 19 on worker 4
    8| #1 of 3:  [dbg-lvish] getLV (active): second frozen check 19 on worker 2
    8| #1 of 3:  [dbg-lvish] putLV: UN-setStatus 19 on worker 1
    8| #2 of 3:  [dbg-lvish] putLV: UN-setStatus 19 on worker 3
    10| #2 of 3:  [dbg-lvish] putLV: calling each listener's onUpdate, 19 on worker 1
    7| #1 of 3:  [dbg-lvish] getLV (active): second globalThresh check 19 on worker 2
    8| #3 of 3:  [dbg-lvish] putLV: read final status before unsetting, 19 on worker 4
    10| #3 of 3:  [dbg-lvish] putLV: calling each listener's onUpdate, 19 on worker 3
    8| #3 of 3:  [dbg-lvish] putLV: UN-setStatus 19 on worker 4
    ! Exception on Logger thread:  [Logger] Need in-parallel log messages to have an ordering, got two equal:
      [dbg-lvish] getLV (active): callback: check thresh 19 on worker 2

How can they both be on worker 2!  Oh, that uniq suf was grabbed too early.

Still, it looks like LOTS of things are racing here.  The second
globalThresh poll, plus MULTIPLE onUpdate checks.  

----------------------------------------

There it is!  A little bit more debugging prints and we got a very nice example of a bad-interleaving.

    7| #1 of 4:  [dbg-lvish] getLV: first readIORef , lv 19 on worker 2
    8| #3 of 3:  [dbg-lvish] putLV: setStatus,, lv 19 on worker 6
    8| #3 of 3:  [dbg-lvish] putLV: setStatus,, lv 19 on worker 5
    8| #4 of 4:  [dbg-lvish] putLV: setStatus,, lv 19 on worker 1
    7| #1 of 3:  [dbg-lvish] getLV (active): check globalThresh, lv 19 on worker 2
    5| #3 of 3:  [dbg-lvish] putLV: about to mutate lvar, lv 19 on worker 6
    5| #3 of 3:  [dbg-lvish] putLV: about to mutate lvar, lv 19 on worker 5
    8| #3 of 3:  [dbg-lvish] putLV: read final status before unsetting, lv 19 on worker 6
    5| #2 of 3:  [dbg-lvish] putLV: about to mutate lvar, lv 19 on worker 1
    4| #1 of 3:  [dbg-lvish] getLV: blocking on LVar, registering listeners, lv 19 on worker 2
    8| #3 of 3:  [dbg-lvish] putLV: read final status before unsetting, lv 19 on worker 5
    8| #1 of 3:  [dbg-lvish] getLV (active): second frozen check, lv 19 on worker 2
    8| #2 of 3:  [dbg-lvish] putLV: UN-setStatus, lv 19 on worker 6
    7| #1 of 3:  [dbg-lvish] getLV (active): second globalThresh check, lv 19 on worker 2
    8| #1 of 3:  [dbg-lvish] putLV: UN-setStatus, lv 19 on worker 5
    9| #2 of 3:  [dbg-lvish] putLV: calling each listener's onUpdate, lv 19 on worker 6
    7| #1 of 3:  [dbg-lvish] getLV (active): second globalThresh tripped, remove tok, lv 19 on worker 2
    7| #1 of 3:  [dbg-lvish] getLV (active): callback: check thresh, lv 19 on worker 6
    9| #2 of 3:  [dbg-lvish] putLV: calling each listener's onUpdate, lv 19 on worker 5
    8| #2 of 3:  [dbg-lvish] getLV (active): read execFlag for dedup, lv 19 on worker 6
    5| #1 of 2:  [dbg-lvish] freezeLV: atomic modify status, lv 19 on worker 2
    8| #2 of 2:  [dbg-lvish] putLV: read final status before unsetting, lv 19 on worker 1
    8| #2 of 2:  [dbg-lvish] getLV (active): CAS execFlag dedup, lv 19 on worker 6
    8| #2 of 2:  [dbg-lvish] putLV: UN-setStatus, lv 19 on worker 1
    7| #2 of 2:  [dbg-lvish] freezeLV: begin busy-wait for
      Exception inside child thread "worker thread", ThreadId 8: PutAfterFreezeExn "Attempt to change a frozen LVar"    
    putter status, lv 19 on worker 2

It seems that the key problem is the putLV "read final status" bit
happening long after the actual mutation, thus racing with the freeze.  

Hmm, it seems like the intent was for the status to serve as a form of
lock.  But in that case freezeLV seems wrong because it doesn't wait
until those "locks" are released before mutating the status...

Do we perhaps need a three-state protocol?  Active->Freezing->Frozen

A put finishing while in Freezing state is ok, but a put beginning
while in Freezing state is bad.  

[2013.12.30] {A problem with the current logging framework}
-----------------------------------------------------------

Right now the protocol is that the "logger" field never gets set when
dbgLvl < 1.  But that is a naughty, dirty thing, and right now it's
leading to this problem:

      i1: [Failed]
    Got the wrong exception, expected one of the strings: ["Attempt to change a frozen LVar"]
    Instead got this exception:
      "LVarSpecificExn \"EXCEPTION in runPar(ThreadId 5): Internal error: Sched logger read before initialized.\""

Hmm, where was that call?


[2013.12.31] {Trying stressTest for the first time}
---------------------------------------------------

I'm getting "blocked indefinitely" errors.  But, I can never get them
from running one test once.  I'm working on AddRemoveSetTests, and I
can get these errors running v3 & v4 together in one process (but not
individually).  OR I can get it with v3 alone but ONLY when I turn the
#reps up to 8.  Eight seems to be the magic number.  I can run
hundreds of times with 7 reps, and I never see the exception.

Actually, this makes sense because it must relate to whether a GC is
triggered that will catch the blocked-indefinitely business.

Yep, that did it.  Performing GC after testing (which is tricky
because test-framewrok tries to exit the process) will force the
errors out.

Hmm.. For thread hygiene ideally test-framework would enforce that
tests don't leave stray threads running.  But checking that would
require being able to enumarate the global set of threads....

-----------

Ok, I fixed some old dependencies on numCapabilities and things are
working better now (idling dependended on it).

Still, right now I can get an actual deadlock if I stress-test at 500
reps.  Also, I can get a detected blocked-indefinitely on v3:

      v3: [Failed]
    ERROR: thread blocked indefinitely in an MVar operation
    ....................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................  v4: [Running]

Ok, now if I can get log printing on failures, that would help us
figure this one out.


[2014.01.23] {A new problem when compiling cfa}
-----------------------------------------------

Haven't seen this one before, a compile failure on the cfa package:

    Failed to install k-cfa-lvish-example-0.1.0.0
    Last 10 lines of the build log ( /ffh/ryan/cloud_drive/working_copies/lvars/lvars/haskell/.cabal-sandbox/logs/k-cfa-lvish-example-0.1.0.0.log ):
          _snYS_info in Main.o
          _soay_info in Main.o
          _soh6_info in Main.o
          _sonF_info in Main.o
          _Main_zdsinsertzuzdsgo1_srt in Main.o
          _Main_zdsfilterGtzufilterzq_srt in Main.o
          _Main_zdsfilterLtzufilterzq_srt in Main.o
          ...
    ld: symbol(s) not found for architecture x86_64

[2014.01.24] {More scheduler debugging}
----------------------------------------

Seeing failures right now on multiple threads on v9f1 v9f2 v9g and (i
think) mc2.


[2014.01.30] {Working on stressTest and the logging framework}
--------------------------------------------------------------

Right now I'm having trouble getting, e.g., AddRemoveSetTests' v3
working.  It deadlocks with the new WaitNum method.


[2014.01.31] {Still some spurious duplication, issue #70}
---------------------------------------------------------

Here's an example:

    |2| wrkr0 waitRemovedSize: about to block.
    |2| wrkr0 PureSet.waitSize: about to (potentially) block:
    |7| wrkr0  [dbg-lvish] getLV: first readIORef , lv 19 on worker 0
    |7| wrkr0  [dbg-lvish] getLV (active): check globalThresh, lv 19 on worker 0
    |8| wrkr2  [dbg-lvish] putLV: initial lvar status read, lv 19 on worker 2
    |8| wrkr2  [dbg-lvish] putLV: setStatus,, lv 19 on worker 2
    |5| wrkr2  [dbg-lvish] putLV: about to mutate lvar, lv 19 on worker 2
    |8| wrkr2  [dbg-lvish] putLV: read final status before unsetting, lv 19 on worker 2
    |8| wrkr0  [dbg-lvish] getLV 20: blocking on LVar, registering listeners...
    |8| wrkr2  [dbg-lvish] putLV: UN-setStatus, lv 19 on worker 2
    |9| wrkr2  [dbg-lvish] putLV: calling each listener's onUpdate, lv 19 on worker 2
    |7| wrkr2  [dbg-lvish] getLV (active): callback: check thresh, lv 19 on worker 2
    |8| wrkr0  [dbg-lvish] getLV (active): second frozen check, lv 19 on worker 0
    |7| wrkr0  [dbg-lvish] getLV (active): second globalThresh check, lv 19 on worker 0
    |7| wrkr0  [dbg-lvish] getLV (active): second globalThresh tripped, remove tok, lv 19 on worker 0
    |8| wrkr2  [dbg-lvish] getLV 20 on worker 2: winner check? True
    |8| wrkr0  [dbg-lvish] getLV 20 on worker 0: winner check? True
    |7| Starting pushWork on worker 2
    |2| wrkr0 waitRemovedSize: unblocked, returning.
    |2| wrkr2 waitRemovedSize: unblocked, returning.
    |5| wrkr0  [dbg-lvish] freezeLV: atomic modify status to Freezing, lv 20 on worker 0
    |5| wrkr2  [dbg-lvish] freezeLV: atomic modify status to Freezing, lv 20 on worker 2
    |7| !cpu 1 woken up

Rather than debug this, it may be better to test whether the non-idem branch does better.


[2014.05.01] {Still seeing problems with lotsaPar test}
--------------------------------------------------------

[2014.10.22] {Debugging unit tests}
----------------------------------------

I'm trying to reactivat the SLMap tests.  Now "-fgetonce" is the
default, so that should fix some problems with duplication.  But what
is the deal with these exceptions?  Work dupliction does not seem
sufficient to explain them.

      Common:
	SLMapTests:
	  traverse: [Failed]
    *** Failed! Exception: 'ConflictingPutExn "Multiple puts to one entry in an IMap!"' (after 2 tests):

      v8d: [Failed]
     ERROR: ConflictingPutExn "Multiple puts to one entry in an IMap!"

Argh, and I just turned on -fdebug, in addition to making -fgetonce
default, and I got this:

      v3e: [Failed]
    ERROR: Final continuation of Par computation was duplicated, in spite of GET_ONCE!
      v8a: [OK]
      v8b: [Failed]
    ERROR: Final continuation of Par computation was duplicated, in spite of GET_ONCE!


[2014.10.24] {Criterion Microbenchmarking}
------------------------------------------

Each pureset insert is doing A LOT of allocation.  When inserting
between 1-260K Ints, the average is 781ns/insert with 1835 bytes.

newFromList on 10 elements does WAY less allocation than 10 inserts...

SLSet microbenchmarks are WAY slower than PureSet.
PureSet might benefit from atomicModifyIORefCAS_ ...

SLSet fillN seems to have a non-linear uptick around 41.6K inserts.
(Cache?  Hmm, L3 cache could have blown before that...)

----------------------------------------

Here's an example of the inner loop when benchmarking PureSet/new.
Honestly, it looks as good as it could be.  Each LVar currently
allocates several mutable locations.  (But I thought it would be
3.. how'd it get to 4?)

It's taking 17.25ns / 128-bytes currently.  It's the 128 bytes of
allocation that seems excessive.

    (\ (w_scwM :: () -> ClosedPar) ->
		       letrec {
			 $wa_scwR :: Int# -> (() -> ClosedPar) -> ClosedPar
			 $wa_scwR =
			   \ (ww_scwP :: Int#) (w1_Xcxf :: () -> ClosedPar) ->
			     case tagToEnum# (># ww_scwP x#_a9Y7) of _ {
			       False ->
				 let {
				   a_scut :: Int#
				   a_scut = +# ww_scwP 1 } in
				 (\ (eta1_B2 :: SchedState) (eta2_XB :: State# RealWorld) ->
				    case newMutVar# (Nothing) eta2_XB
				    of _ { (# ipv_aaNi, ipv1_aaNj #) ->
				    case newMutVar# (Nil) ipv_aaNi of _ { (# ipv2_aaMP, ipv3_aaMQ #) ->
				    case newMutVar# (Active ((STRef ipv3_aaMQ) `cast` ...)) ipv2_aaMP
				    of _ { (# ipv4_aaMU, ipv5_aaMV #) ->
				    case newMutVar# () ipv4_aaMU of _ { (# ipv6_aaMZ, ipv7_aaN0 #) ->
				    (((($wa_scwR a_scut w1_Xcxf) `cast` ...) eta1_B2) `cast` ...)
				      ipv6_aaMZ
				    }
				    }
				    }
				    })
				 `cast` ...;
			       True -> w1_Xcxf ()
			     }; } in
		       $wa_scwR 1 w_scwM)

If we proceed to STG, where let=allocation and case=evaluation, then
we see three let's in the loop.  The first is for the (SchedState ->
...) function, and then the other two are:

	 let {
	   sat_scyg
	     :: Bag
		  (Listener
		     Any) =
	       NO_CCS STRef! [ipv3_scyf]; } in
	 let {
	   sat_scyh
	     :: Status
		  Any =
	       NO_CCS Active! [sat_scyg];

How about if we look at the "fill10" example instead?  It posts up a
whopping 8700 bytes of allocation (and ~2 microseconds / 5.6K cycles).
(A regular Data.Set.fromList on [1..10] takes 600 cycles and does 700
bytes alloc.  Huh, it looks like that version is specializing on
[Int]... )

This one clearly only does the 4 newMutVar calls once, and then goes
into "$wloop_".  Our "insert" inlined but the Data.Set insert did not,
requiring us to box the Int on the way off to that library function.

One immediate difference from the above is that "Par" did not go away
in exposing the "ClosedPar" underneath in this one.  Presumably
because this is a loop IN the par monad rather than the IO monad....

    letrec {
      $wloop_scNC :: Int# -> Par Full Any ()
      $wloop_scNC =
	\ (ww1_scNA :: Int#) ->
	  let {
	    i_X9jQ :: Int
	    i_X9jQ = I# ww1_scNA } in
	  let {
	    lvl6_scP1 :: Maybe Int
	    lvl6_scP1 = Just i_X9jQ } in
	  case tagToEnum# (># ww1_scNA 10) of _ {
	    False ->
	      let {
		a1_sawc :: Par Full Any ()
		a1_sawc = $wloop_scNC (+# ww1_scNA 1) } in
	      let {
		m1_aasA :: Par ()
		m1_aasA =
		  let {
		    lvl7_scP4 :: (Set Int, Maybe Int)
		    lvl7_scP4 = (Tip, lvl6_scP1) } in
		  let {
		    lvl8_scP6 :: Set Int -> (Set Int, Maybe Int)
		    lvl8_scP6 =
		      \ (set_aach :: Set Int) ->
			case insert $fOrdInt i_X9jQ set_aach
			of wild4_aacG {
			  Bin dt_aacI ds4_aacJ ds5_aacK ds6_aacL ->
			    case set_aach of wild5_aacn {
			      Bin dt1_aacp ds7_aacq ds8_aacr ds9_aacs ->
				case tagToEnum# (># dt_aacI dt1_aacp)
				of _ {
				  False -> (wild5_aacn, Nothing);
				  True -> (wild4_aacG, lvl6_scP1)
				};
			      Tip ->
				case tagToEnum# (># dt_aacI 0) of _ {
				  False -> lvl_rcGu;
				  True -> (wild4_aacG, lvl6_scP1)
				}
			    };
			  Tip ->
			    case set_aach of wild5_aacn {
			      Bin dt_aacp ds4_aacq ds5_aacr ds6_aacs ->
				case tagToEnum# (># 0 dt_aacp) of _ {
				  False -> (wild5_aacn, Nothing);
				  True -> lvl7_scP4
				};
			      Tip -> lvl_rcGu
			    }
			} } in
		  $wputLV_
		    (a_scvn `cast` ...)
		    ipv5_aaPg
		    ipv7_aaPl
		    ((\ (a3_acwW :: IORef (Set Int))
			(eta3_B3 :: (Maybe Int, ()) -> ClosedPar)
			(eta4_XY :: SchedState)
			(eta5_X1V :: State# RealWorld) ->
			case a3_acwW `cast` ... of _ { STRef ww3_aavD ->
			case $wa ww3_aavD lvl8_scP6 eta5_X1V
			of _ { (# ipv8_acx3, ipv9_acx4 #) ->
			((((eta3_B3 (ipv9_acx4, ())) `cast` ...)
			    eta4_XY)
			 `cast` ...)
			  ipv8_acx3
			}
			})
		     `cast` ...) } in
	      (\ (eta3_Xato :: () -> ClosedPar) ->
		 let {
		   lvl7_scP7 :: ClosedPar
		   lvl7_scP7 = (a1_sawc `cast` ...) eta3_Xato } in
		 (m1_aasA `cast` ...) (\ _ -> lvl7_scP7))
	      `cast` ...;
	    True -> lvl1_rcQD `cast` ...
	  }; }

