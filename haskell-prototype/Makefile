all := BFS test

ifeq ($(GHC),)
 GHC=ghc
endif

all: $(all)

%: %.hs
	$(GHC) -O2 -threaded -rtsopts -eventlog --make $< -o $@

pure: %.hs
	$(GHC) -O2 -DPURE -threaded -rtsopts -eventlog --make $< -o $@

test:

clean:
	-rm -f *.hi *.o *.html *.eventlog $(all)
