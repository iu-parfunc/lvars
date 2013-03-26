all := BFS_impure BFS_pure pp_impure pp_pure test

ifeq ($(GHC),)
 GHC=ghc
endif

all: $(all)

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

test:

clean:
	-rm -f *.hi *.o *.html *.eventlog $(all)
