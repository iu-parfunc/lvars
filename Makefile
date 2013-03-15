all := BFS test

all: $(all)

%: %.hs
	ghc -O2 -threaded --make $<

clean:
	-rm -f *.hi *.o *.html $(all)