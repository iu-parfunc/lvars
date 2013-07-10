

# fixing it to this table:
TABLEID=1YxEmNpeUoGCBptDK0ddtomC_oK2IVH1f2M89IIA
CREDENTIALS= --clientid=905767673358.apps.googleusercontent.com --clientsecret=2a2H57dBggubW1_rqglC7jtK

run: build
	./run_benchmarks.exe --fusion-upload=$(TABLEID) --name=LVish $(CREDENTIALS)

# -threaded is important here:
build:
	ghc -threaded run_benchmarks.hs -o run_benchmarks.exe

clean:
	rm -f *.hi *.o *.exe

distclean: clean
	rm -f *.dat *.log *.dat.* *.log.*

