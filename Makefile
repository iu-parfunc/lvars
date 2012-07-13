

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
	./dist/build/test-lambdapar/test-lambdapar -j1

q: quick
quick:
	ghc -package ghc --make UniqueDesugar -o UniqueDesugar.exe
	ghc -package ghc --make RunTests      -o RunTests.exe
#	ghc -package ghc --make Language/LambdaPar/RaceDet1 -o UniqueDesugar.exe

test-junit:
	${CABAL} clean
# This won't work with cabal-dev or older cabal:
#	${CABAL} configure --enable-tests
#	${CABAL} build
	${CABAL} install --with-ghc=$(GHC)
	./dist/build/test-lambdapar/test-lambdapar -j1 --jxml=test-lambdapar.xml

clean:
	${CABAL} clean
	rm -f *.hi Language/LambdaPar/*.hi
	rm -f *.o Language/LambdaPar/*.o
	rm -f RunTests UniqueDesugar Language/LambdaPar/RaceDet1

