all := BFS_impure BFS_pure pp_impure pp_pure test

# 97336 vertices:
DATABIG    = /tmp/grid
DATAMEDIUM = /tmp/grid_8000
DATASMALL  = /tmp/grid_1000

PBBSDIR = ../pbbs/testData/graphData

ifeq ($(GHC),)
 GHC=ghc
endif

all: $(all) data

%: %.hs
	$(GHC) -O2 -threaded -rtsopts -eventlog --make $< -o $@

BFS_pure: BFS.hs
	$(GHC) -O2 -DPURE -threaded -rtsopts -eventlog --make BFS.hs -o BFS_pure

BFS_impure: BFS.hs
	$(GHC) -O2 -threaded -rtsopts -eventlog --make BFS.hs -o BFS_impure

pp_pure: pleasantly_par.hs
	$(GHC) -O2 -DPURE -threaded -rtsopts -eventlog --make pleasantly_par.hs -o pp_pure

pp_impure: pleasantly_par.hs
	$(GHC) -O2 -threaded -rtsopts -eventlog --make pleasantly_par.hs -o pp_impure

#--------------------------------------------------------------------------------
# Input data generation:

data: $(DATABIG) $(DATAMEDIUM) $(DATASMALL)

# Generate test data using PBBS.
$(DATABIG):
	(cd $(PBBSDIR); make gridGraph)
	$(PBBSDIR)/gridGraph -d 3 97336 $(DATABIG) # 46^3 -- switch to 125000 (50^3), maybe?

$(DATAMEDIUM):
	(cd $(PBBSDIR); make gridGraph)
	$(PBBSDIR)/gridGraph -d 3 8000 $(DATAMEDIUM) # 20^3

$(DATASMALL):
	(cd $(PBBSDIR); make gridGraph)
	$(PBBSDIR)/gridGraph -d 3 1000 $(DATASMALL) # 10^3

#--------------------------------------------------------------------------------
# Ryan's quick hacks:

q: quick.exe
quick.exe: BFS.hs
	$(GHC) -DNOCRITERION -O2 -threaded -rtsopts --make BFS.hs -o $@

qp: quick_pure.exe
quick_pure.exe: BFS.hs
	$(GHC) -DPURE -DNOCRITERION -O2 -threaded -rtsopts --make BFS.hs -o $@

test: q qp
	time ./quick.exe +RTS -N1
	time ./quick.exe +RTS -N4
	@echo "\nPURE VERSION"
	@echo "=============================================================="
	time ./quick_pure.exe +RTS -N1
	time ./quick_pure.exe +RTS -N4

clean:
	-rm -f *.hi *.o *.html *.eventlog *.exe $(all) 
