all := BFS.exe test

ifeq ($(GHC),)
 GHC=ghc
endif

all: $(all)

%.exe: %.hs
	$(GHC) -O2 -threaded --make $< -o $@

test:

clean:
	-rm -f *.hi *.o *.html $(all)
