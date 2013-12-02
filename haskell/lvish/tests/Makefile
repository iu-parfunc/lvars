# A simple convenience for building tests without Cabal.

# LIBSRC = $(shell find ../Data/ -name "*.hs") \
#          $(shell find ../Control/ -name "*.hs")
LIBSRC =

ARGS = -threaded -DDEBUG_LVAR $(GHC_ARGS)
# -fforce-recomp

 .SUFFIXES: .hs .exe

all: ArrayTests.exe GenericTests.exe LVishAndIVar.exe LogicalTests.exe MapTests.exe MaxCounterTests.exe MemoTests.exe SNZITests.exe SetTests.exe SkipListTests.exe Main.exe 

.hs.exe:
	ghc $(ARGS) -i.. -main-is $(^:.hs=.runTests) $^ -o $@

main: Main.exe
Main.exe:
	ghc $(ARGS) -i.. Main.hs -o Main.exe

clean: 
	rm -f *.hi *.o SkipListTests.exe Main.exe
