all := BFS_impure BFS_pure pp_impure pp_pure test

DATA = /tmp/grid
DATASMALL = /tmp/grid_small
PBBSDIR = ../pbbs/testData/graphData/

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

data: $(DATA) $(DATASMALL)

# Generate test data using PBBS.
$(DATA):
	(cd $(PBBSDIR); make gridGraph)
	$(PBBSDIR)/gridGraph -d 3 100000 $(DATA)

$(DATASMALL):
	(cd $(PBBSDIR); make gridGraph)
	$(PBBSDIR)/gridGraph -d 3 1000 $(DATASMALL)

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
	-rm -f *.hi *.o *.html *.eventlog $(all)
