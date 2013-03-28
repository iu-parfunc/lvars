src = LVarTracePure.hs LVarTraceIO.hs BFS_LVar.hs BFS_Strategies.hs Runner.hs Common.hs

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
# Input data generation:

data: grid_data rand_data

grid_data: $(GRID_DATABIG) $(GRID_DATAMEDIUM) $(GRID_DATASMALL)

rand_data: $(RAND_DATA)

# Generate test data using PBBS.

# 3D grids.
$(GRID_DATABIG):
	(cd $(PBBSDIR); make gridGraph)
	$(PBBSDIR)/gridGraph -d 3 125000 $(GRID_DATABIG) # 50^3

$(GRID_DATAMEDIUM):
	(cd $(PBBSDIR); make gridGraph)
	$(PBBSDIR)/gridGraph -d 3 8000 $(GRID_DATAMEDIUM) # 20^3

$(GRID_DATASMALL):
	(cd $(PBBSDIR); make gridGraph)
	$(PBBSDIR)/gridGraph -d 3 1000 $(GRID_DATASMALL) # 10^3

# Random local graph with 10,000 vertices and approximately 5 edges
# per vertex (50,000 edges total), and dimension 5.  The graph is
# generated so that the probability of each edge out of a vertex is
# biased to nearby vertices.
$(RAND_DATA):
	(cd $(PBBSDIR); make randLocalGraph)
	$(PBBSDIR)/randLocalGraph -m 50000 -d 5 10000 $(RAND_DATA)

#--------------------------------------------------------------------------------

l1: lvar_io.exe
lvar_io.exe: $(src)
	$(GHC) -O2 -threaded -rtsopts --make BFS_LVar.hs -o $@

l2: lvar_pure.exe
lvar_pure.exe: $(src)
	$(GHC) -DPURE -O2 -threaded -rtsopts --make BFS_LVar.hs -o $@

s: strategies.exe
strategies.exe: $(src)
	$(GHC) -O2 -threaded -rtsopts --make BFS_Strategies.hs -o $@

benchmark.run:
	ghc --make benchmark.hs -o benchmark.run

clean:
	-rm -f *.hi *.o *.html *.eventlog *.exe $(all) 
# /tmp/grid* /tmp/rand*

wipelogs:
	-rm -f *.log
