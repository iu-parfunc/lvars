# Benchmarking results

This directory contains raw log files from the benchmarks we ran for
our FHPC 2013 submission, plus some code for visualizing the data.

We compared two programs (Strategies and LVarPure versions of
bf_traverse), varying the number of cores (1, 2, 3, 4), and the
microseconds of work done per node (1, 2, 4, 8, 16, and 32), for a
total of 2 * 4 * 6 = 48 configurations.  We ran each configuration for
5 trials.

The file `results_basalt.dat` contains a summary of the results: the
minimum, median, and maximum running time for each trial, as well as
the minimum, median, and maximum productivity for each trial.  The
file `bench_basalt.log` contains the complete log of results.
(`basalt` is the name of the machine these tests were run on.)

`bf_traverse_benchmark_data.csv` is a pared-down, easier-to-parse
version of `results_basalt.dat`, and `makegraph.py` is the script that
produces `bf_traverse_benchmark_data.png`.  It produces four subplots,
one for each number of cores.  The 4-core version appears in our
paper.