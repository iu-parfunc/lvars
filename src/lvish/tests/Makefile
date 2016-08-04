# A simple convenience for building tests without Cabal
# (and WITH debugging enabled).
#------------------------------------------------------

# LIBSRC = $(shell find ../Data/ -name "*.hs") \
#          $(shell find ../Control/ -name "*.hs")
LIBSRC =

ARGS = -DGET_ONCE=1 -rtsopts -threaded -DDEBUG_LVAR $(GHC_ARGS)
ARGS += -O2 -eventlog
# -fforce-recomp

ALLEXES = AddRemoveSetTests.exe ArrayTests.exe GenericTests.exe LVishAndIVar.exe LogicalTests.exe PureMapTests.exe SLMapTests.exe MaxPosIntTests.exe MemoTests.exe PNCounterTests.exe SNZITests.exe SetTests.exe SkipListTests.exe BulkRetryTests.exe Main.exe Main_wfailing.exe Current.exe

 .SUFFIXES: .hs .exe

all: $(ALLEXES)

prof: 
	$(MAKE) MapTests.exe
	rm -f MapTests.exe
	GHC_ARGS="-prof -auto-all -osuf=po" $(MAKE) MapTests.exe

.hs.exe: CommonMapTests.hs
	ghc $(ARGS) -i.. -main-is $(^:.hs=.runTests) $^ -o $@

main: Main.exe Main_wfailing.exe
Main.exe:
	ghc $(ARGS) -i.. Main.hs -o Main.exe

Main_wfailing.exe:
	ghc -DFAILING_TESTS $(ARGS) -i.. Main.hs -o Main_wfailing.exe

clean: 
	rm -f *.hi *.o $(ALLEXES) 
