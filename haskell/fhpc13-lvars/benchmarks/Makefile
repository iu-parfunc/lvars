# A Makefile for generating the input data we use for benchmarks.
# It's easier to use this instead of just building everything with
# cabal.

GRID_DATABIG    = /tmp/grid_125000
GRID_DATAMEDIUM = /tmp/grid_8000
GRID_DATASMALL  = /tmp/grid_1000

RAND_DATA       = /tmp/rand

PBBSDIR = ../../../pbbs/testData/graphData

gen_data: grid_data rand_data

grid_data: $(GRID_DATABIG) $(GRID_DATAMEDIUM) $(GRID_DATASMALL)

rand_data: $(RAND_DATA)

clean_data:
	-rm -f /tmp/grid* /tmp/rand*

# Generate 3D grid graphs with one of [1,000, 8,000, 125,000]
# vertices.  (We aren't actually using these for anything at the
# moment.)

$(GRID_DATASMALL):
	(cd $(PBBSDIR); make gridGraph)
	$(PBBSDIR)/gridGraph -d 3 1000 $(GRID_DATASMALL) # 10^3

$(GRID_DATAMEDIUM):
	(cd $(PBBSDIR); make gridGraph)
	$(PBBSDIR)/gridGraph -d 3 8000 $(GRID_DATAMEDIUM) # 20^3

$(GRID_DATABIG):
	(cd $(PBBSDIR); make gridGraph)
	$(PBBSDIR)/gridGraph -d 3 125000 $(GRID_DATABIG) # 50^3

# Generate random local graphs with one of [10,000; 20,000; 40,000]
# vertices and one of [40,000; 80,000; 160,000] edges, and dimension
# 5.  The graph is generated so that the probability of each edge out
# of a vertex is biased to nearby vertices.  Overall, a graph with,
# say, 20,000 vertices and 80,000 edges will have an *average* degree
# of 4. (The only one of these we're using at the moment is the one

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
