

ifeq ($(CABAL),)
  CABAL=cabal
endif

ifeq ($(GHC),)
  GHC=ghc
endif

test:
	${CABAL} clean
# This won't work with cabal-dev or older cabal:
#	${CABAL} configure --enable-tests
#	${CABAL} build
	${CABAL} install --with-ghc=$(GHC)
	./dist/build/test-lambdapar/test-lambdapar -j1

test-junit:
	${CABAL} clean
# This won't work with cabal-dev or older cabal:
#	${CABAL} configure --enable-tests
#	${CABAL} build
	${CABAL} install --with-ghc=$(GHC)
	./dist/build/test-lambdapar/test-lambdapar -j1 --jxml=test-lambdapar.xml

clean:
	${CABAL} clean
	rm -f Language/LambdaPar/*.hi
	rm -f Language/LambdaPar/*.o
