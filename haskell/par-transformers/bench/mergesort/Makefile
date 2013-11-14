
ifeq ($(GHC),)
  GHC=ghc
endif

CC=icc
LDFLAGS= -lcilkrts -lirc -limf

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


SANDBOX_HACK= -no-user-package-db -package-db ../../../.cabal-sandbox/x86_64-linux-ghc-7.6.3-packages.conf.d

MergeSort.exe: MergeSort.hs cilksort-nomain.o
	$(GHC) $(SANDBOX_HACK) $(COMPILE_ARGS) -i../.. --make -O2 -rtsopts -threaded $^ -lm $(LDFLAGS) -o $@

cilksort-nomain.o: cilksort.c
	icc $(CFLAGS) -c $^ -o $@
#	bash -c "(module add intel; icc $(CFLAGS) -c $^ -o $@)"

# cilk: 
# 	$(MAKE) cilksort-nomain.o LDFLAGS="-lcilkrts -lirc -limf"

# This runs the benchmark
run:
	./MergeSort.exe $(RUN_ARGS)
# HACK:
#	bash -c "(module add intel; ./MergeSort.exe $(RUN_ARGS))"

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
