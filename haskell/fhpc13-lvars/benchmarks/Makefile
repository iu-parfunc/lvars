# A Makefile for generating the input data we use for benchmarks.
# It's easier to use this instead of just building everything with
# cabal.

VERTICES   =  40000
EDGES      = 320000

RAND_DATA  = /tmp/rand

PBBSDIR    = ../../../pbbs/testData/graphData

# Generate random local graphs with $VERTICES vertices and $EDGES
# edges, and dimension 5.  The graph is generated so that the
# probability of each edge out of a vertex is biased to nearby
# vertices.

# The resulting file ends up in /tmp/rand_E_V where E is the number of
# edges and V is the number of vertices.

default: rand_data

rand_data:
	(cd $(PBBSDIR); make randLocalGraph)
	$(PBBSDIR)/randLocalGraph -m $(EDGES) -d 5 $(VERTICES) $(RAND_DATA)_$(EDGES)_$(VERTICES)

clean_data:
	-rm -f /tmp/rand*
