
ifeq ($(GHC),)
  GHC=ghc
endif

ifeq ($(CC),gcc)
  CC=gcc
  LDFLAGS=-lm 
else
  CC=icc
  LDFLAGS=-lm -lcilkrts -lirc -limf
endif

CFLAGS= -Wall -O3

# ifeq ($(CPU_SORT),)
#   CPU_SORT=HASKELL_SEQ
# endif

all: MergeSort.exe

# lvish-st:
# 	$(MAKE) WHICHSCHED="Control.LVish"

# trace-st:
# 	$(MAKE) WHICHSCHED="Control.Monad.Par.Scheds.Trace"

# sparks-st:
# 	$(MAKE) WHICHSCHED="Control.Monad.Par.Scheds.Sparks"


MergeSort.exe: MergeSort.hs cilksort-nomain.o
	$(GHC) $(COMPILE_ARGS) -i../.. --make -O2 -rtsopts -threaded $^ $(LDFLAGS) -o $@

cilksort-nomain.o: cilksort.c
	module add intel
	icc $(CFLAGS) -c $^ -o $@

# This runs the benchmark
run:
	./MergeSort.exe $(RUN_ARGS)

clean:
	rm -f cilkWrap mergesort cilksort *.hi *.o

.PHONY: clean

#--------------------------------------------------------------------------------
# TRASH

cilkWrap: cilkWrap.hs cilksort-nomain.o
	$(GHC) --make -O2 $^ $(LDFLAGS) -o $@

cilksort.o: cilksort.c
	$(CC) $(CFLAGS) -DUSE_MAIN=1 -c $^ -o $@

cilksort: cilksort.o
	$(CC) $(CFLAGS) $^ -o $@
