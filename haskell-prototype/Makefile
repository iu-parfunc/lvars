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

q: quick.exe
quick.exe: BFS.hs
	$(GHC) -DNOCRITERION -O2 -threaded -rtsopts --make BFS.hs -o quick.exe

pp_pure: pleasantly_par.hs
	$(GHC) -O2 -DPURE -threaded -rtsopts -eventlog --make pleasantly_par.hs -o pp_pure

pp_impure: pleasantly_par.hs
	$(GHC) -O2 -threaded -rtsopts -eventlog --make pleasantly_par.hs -o pp_impure

data: $(DATA) $(DATASMALL)

# Generate test data using PBBS.
$(DATA):
	(cd $(PBBSDIR); make gridGraph)
	$(PBBSDIR)/gridGraph -d 3 100000 $(DATA)

$(DATASMALL):
	(cd $(PBBSDIR); make gridGraph)
	$(PBBSDIR)/gridGraph -d 3 1000 $(DATASMALL)

test:

clean:
	-rm -f *.hi *.o *.html *.eventlog $(all)
