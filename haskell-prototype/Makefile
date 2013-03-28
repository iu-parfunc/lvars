src = LVarTracePure.hs LVarTraceIO.hs BFS.hs

DATABIG    = /tmp/grid_125000
DATAMEDIUM = /tmp/grid_8000
DATASMALL  = /tmp/grid_1000

PBBSDIR = ../pbbs/testData/graphData

ifeq ($(GHC),)
 GHC=ghc
endif

default: q qp

%: %.hs
	$(GHC) -O2 -threaded -rtsopts --make $< -o $@

#--------------------------------------------------------------------------------
# Input data generation:

data: $(DATABIG) $(DATAMEDIUM) $(DATASMALL)

# Generate test data using PBBS.
$(DATABIG):
	(cd $(PBBSDIR); make gridGraph)
	$(PBBSDIR)/gridGraph -d 3 125000 $(DATABIG) # 50^3

$(DATAMEDIUM):
	(cd $(PBBSDIR); make gridGraph)
	$(PBBSDIR)/gridGraph -d 3 8000 $(DATAMEDIUM) # 20^3

$(DATASMALL):
	(cd $(PBBSDIR); make gridGraph)
	$(PBBSDIR)/gridGraph -d 3 1000 $(DATASMALL) # 10^3

#--------------------------------------------------------------------------------

q: quick_io.exe
quick_io.exe: $(src)
	$(GHC) -O2 -threaded -rtsopts --make BFS.hs -o $@

qp: quick_pure.exe
quick_pure.exe: $(src)
	$(GHC) -DPURE -O2 -threaded -rtsopts --make BFS.hs -o $@

test: q qp
	time ./quick_io.exe +RTS -N1
	time ./quick_io.exe +RTS -N4
	@echo "\nPURE VERSION"
	@echo "=============================================================="
	time ./quick_pure.exe +RTS -N1
	time ./quick_pure.exe +RTS -N4

clean:
	-rm -f *.hi *.o *.html *.eventlog *.exe $(all) 
