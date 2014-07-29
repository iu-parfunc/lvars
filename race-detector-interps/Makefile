

ifeq ($(CABAL),)
  CABAL=cabal
endif

ifeq ($(GHC),)
  GHC=ghc
endif

default:
	${CABAL} install --with-ghc=$(GHC)

test:
	${CABAL} clean
	${CABAL} install --with-ghc=$(GHC)
	./dist/build/test-lambdaLVar/test-lambdaLVar -j1

q: quick
quick:
	ghc -package ghc --make RunTests      -o RunTests.exe

test-junit:
	${CABAL} clean
# This won't work with cabal-dev or older cabal:
#	${CABAL} configure --enable-tests
#	${CABAL} build
	${CABAL} install --with-ghc=$(GHC) --bindir=.
	./test-lambdaLVar -j1 --jxml=test-lambdaLVar.xml

clean:
	${CABAL} clean
	rm -f *.hi Language/LambdaLVar/*.hi
	rm -f *.o Language/LambdaLVar/*.o
	rm -f RunTests UniqueDesugar Language/LambdaLVar/RaceDet1
	rm test-lambdaLVar.xml

