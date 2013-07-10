

run: build
	./run_benchmarks.exe --fusion-upload --name=LVish --clientid=905767673358.apps.googleusercontent.com --clientsecret=2a2H57dBggubW1_rqglC7jtK

# -threaded is important here:
build:
	ghc -threaded run_benchmarks.hs -o run_benchmarks.exe

clean:
	rm -f *.hi *.o *.exe

distclean: clean
	rm -f *.dat *.log *.dat.* *.log.*

