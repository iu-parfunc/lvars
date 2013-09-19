

DBGOPTS= -keep-tmp-files -dsuppress-module-prefixes -ddump-to-file -ddump-core-stats -ddump-simpl-stats -dcore-lint \
 -dcmm-lint -ddump-ds -ddump-simpl -ddump-stg -ddump-asm -ddump-bcos -ddump-cmm -ddump-opt-cmm -ddump-inlinings \

BASICOPTS= -rtsopts -fforce-recomp -O2 -threaded

OPTS= $(DBGOPTS) $(BASICOPTS)

all:
	rm -f *.o *.hi bugged.exe unbugged.exe
	rm -rf ./bugged_dumps ./unbugged_dumps/
	ghc -DACTIVATE_BUG $(OPTS) PBBS.hs -main-is Util.PBBS.t4 -o bugged.exe
	mkdir -p ./bugged_dumps/
	mv *.dump* ./bugged_dumps/
	ghc                $(OPTS) PBBS.hs -main-is Util.PBBS.t4 -o unbugged.exe
	mkdir -p ./unbugged_dumps/
	mv *.dump* ./unbugged_dumps/
	time ./unbugged.exe
	time ./bugged.exe

unbugged:
#	ghc $(BASICOPTS) PBBS.hs -main-is Util.PBBS.t2 -o unbugged_mmap.exe
#	ghc $(BASICOPTS) PBBS.hs -main-is Util.PBBS.t4 -o unbugged_readFile.exe
	ghc $(BASICOPTS) PBBS.hs -main-is Util.PBBS.t5 -o unbugged_mmap.exe

small:
	ghc $(BASICOPTS) PBBS.hs -main-is Util.PBBS.t0 -o small_mmap.exe


dbg:
	ghc $(BASICOPTS)                PBBS.hs -main-is Util.PBBS.t0 -o small_mmap.exe
	ghc $(BASICOPTS) -osuf=po -prof PBBS.hs -main-is Util.PBBS.t0 -o small_mmap_dbg.exe
