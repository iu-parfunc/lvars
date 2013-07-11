

# fixing it to this table:
TABLEID=1YxEmNpeUoGCBptDK0ddtomC_oK2IVH1f2M89IIA
CREDENTIALS= --clientid=905767673358.apps.googleusercontent.com --clientsecret=2a2H57dBggubW1_rqglC7jtK

# BENCHARGS= --trials=3 --runid=d005_1373532273 --skipto=106
# BENCHARGS= --trials=3 --runid=d003_1373528191 --skipto=137
BENCHARGS= --trials=3 

run: build
	./run_benchmarks.exe --fusion-upload=$(TABLEID) --name=LVish $(CREDENTIALS)

fullrun: build
	./run_benchmarks.exe $(BENCHARGS) --fusion-upload=$(TABLEID) --name=LVish $(CREDENTIALS) CFA

basic: build
	QUICK=1 ./run_benchmarks.exe

# -threaded is important here:
build:
	ghc -threaded run_benchmarks.hs -o run_benchmarks.exe

prof:
	ghc -O0 -fforce-recomp -prof -fprof-auto -fprof-cafs -fprof-auto-calls -threaded run_benchmarks.hs -o run_benchmarks.exe

clean:
	rm -f *.hi *.o *.exe

distclean: clean
	rm -f *.dat *.log *.dat.* *.log.*

