#!/bin/bash

# Just a utility script to handle building and running the benchmarks.

# LK: TODO: figure out if HSBencher needs/uses the TRIALS env var.

set -e

make rand_data

if [ $(hostname) = "hive.soic.indiana.edu" ];
then
    lock_for_experiments 1000
	export TRIALS=5
fi

if [ $(hostname) = "lenny" || $(hostname) = "landin.local" ]; # Lindsey's machines
then
    export TRIALS=1
fi

make benchmark.run
./benchmark.run

if [ $(hostname) = "hive.soic.indiana.edu" ];
then
    unlock_for_experiments
fi

