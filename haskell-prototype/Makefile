all := BFS test

ifeq ($(GHC),)
 GHC=ghc
endif

all: $(all)

%: %.hs
	$(GHC) -O2 -threaded -rtsopts -eventlog --make $< -o $@

test:

clean:
	-rm -f *.hi *.o *.html *.eventlog $(all)
