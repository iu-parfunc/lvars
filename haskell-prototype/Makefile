src = LVarTracePure.hs LVarTraceIO.hs bf_traverse_LVar.hs bf_traverse_Strategies.hs bf_traverse_monad-par.hs Runner.hs Common.hs benchmark.hs

GRID_DATABIG    = /tmp/grid_125000
GRID_DATAMEDIUM = /tmp/grid_8000
GRID_DATASMALL  = /tmp/grid_1000

RAND_DATA       = /tmp/rand

PBBSDIR = ../pbbs/testData/graphData

ifeq ($(GHC),)
 GHC=ghc
endif

default: l1 l2 s

%: %.hs
	$(GHC) -O2 -threaded -rtsopts --make $< -o $@

#--------------------------------------------------------------------------------
# Input data generation: generate graphs using PBBS.

gendata: grid_data rand_data

grid_data: $(GRID_DATABIG) $(GRID_DATAMEDIUM) $(GRID_DATASMALL)

rand_data: $(RAND_DATA)

# 3D grids.  We're not using these for any benchmarks at the moment.
$(GRID_DATABIG):
	(cd $(PBBSDIR); make gridGraph)
	$(PBBSDIR)/gridGraph -d 3 125000 $(GRID_DATABIG) # 50^3

$(GRID_DATAMEDIUM):
	(cd $(PBBSDIR); make gridGraph)
	$(PBBSDIR)/gridGraph -d 3 8000 $(GRID_DATAMEDIUM) # 20^3

$(GRID_DATASMALL):
	(cd $(PBBSDIR); make gridGraph)
	$(PBBSDIR)/gridGraph -d 3 1000 $(GRID_DATASMALL) # 10^3

# Random graphs.

# Generate random local graphs with one of [10,000; 20,000; 40,000]
# vertices and one of [40,000; 80,000; 160,000] edges, and dimension
# 5.  The graph is generated so that the probability of each edge out
# of a vertex is biased to nearby vertices.  Overall, a graph with,
# say, 20,000 vertices and 80,000 edges will have an *average* degree
# of 4.

# We should produce 9 graphs in all.

# Naming convention: /tmp/rand_E_V where E is the number of edges and
# V is the number of vertices.

$(RAND_DATA):
	(cd $(PBBSDIR); make randLocalGraph)
# 40000 edges
	$(PBBSDIR)/randLocalGraph -m  40000 -d 5 10000 $(RAND_DATA)_40000_10000
	$(PBBSDIR)/randLocalGraph -m  40000 -d 5 20000 $(RAND_DATA)_40000_20000
	$(PBBSDIR)/randLocalGraph -m  40000 -d 5 40000 $(RAND_DATA)_40000_40000
# 80000 edges
	$(PBBSDIR)/randLocalGraph -m  80000 -d 5 10000 $(RAND_DATA)_80000_10000
	$(PBBSDIR)/randLocalGraph -m  80000 -d 5 20000 $(RAND_DATA)_80000_20000
	$(PBBSDIR)/randLocalGraph -m  80000 -d 5 40000 $(RAND_DATA)_80000_40000
# 160000 edges
	$(PBBSDIR)/randLocalGraph -m 160000 -d 5 10000 $(RAND_DATA)_160000_10000
	$(PBBSDIR)/randLocalGraph -m 160000 -d 5 20000 $(RAND_DATA)_160000_20000
	$(PBBSDIR)/randLocalGraph -m 160000 -d 5 40000 $(RAND_DATA)_160000_40000
	$(PBBSDIR)/randLocalGraph -m 320000 -d 5 40000 $(RAND_DATA)_320000_40000

deps:
	cabal install rdtsc parallel monad-par lattices hydra-print test-framework test-framework-th HUnit

#--------------------------------------------------------------------------------

l1: lvar_io.exe
lvar_io.exe: $(src)
	$(GHC) -O2 -threaded -rtsopts --make bf_traverse_LVarIO.hs -o $@

l2: lvar_pure.exe
lvar_pure.exe: $(src)
	$(GHC) -O2 -threaded -rtsopts --make bf_traverse_LVarPure.hs -o $@

s: strategies.exe
strategies.exe: $(src)
	$(GHC) -O2 -threaded -rtsopts --make bf_traverse_Strategies.hs -o $@

p: monad-par.exe
monad-par.exe: $(src)
	$(GHC) -O2 -threaded -rtsopts --make bf_traverse_monad-par.hs -o $@

benchmark.run:
	ghc --make benchmark.hs -o benchmark.run

clean:
	-rm -f *.hi *.o *.html *.eventlog *.exe *.run $(all) 

wipelogs:
	-rm -rf *.log *.bak *.dat bin/ build/ runs/ *.out

#--------------------------------------------------------------------------------

test: test_pure.exe test_io.exe test_scalable.exe
test_pure.exe:
	$(GHC) -DLVARPURE -O2 -threaded -rtsopts --make test.hs -o $@
test_io.exe:
	$(GHC) -DLVARIO -O2 -threaded -rtsopts --make test.hs -o $@
test_scalable.exe:
	$(GHC) -DLVARSCALABLE -O2 -threaded -rtsopts --make test.hs -o $@
