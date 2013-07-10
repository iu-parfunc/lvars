

all: one two

one:
	ghc           -fforce-recomp -rtsopts -threaded -O2 0CFA_lvish.hs -o 0CFA_lvish.exe
	ghc -DINPLACE -fforce-recomp -rtsopts -threaded -O2 0CFA_lvish.hs -o 0CFA_lvish_inplace.exe

two:
	ghc -fforce-recomp -rtsopts -threaded -O2 0CFA.hs -o 0CFA.exe
