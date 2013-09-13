#!/bin/bash

set -e

make rand_data

export THREADS="1 2 3 4"
export TRIALS=5

if [ $(hostname) = "hive.soic.indiana.edu" ];
then
    lock_for_experiments 1000
    export THREADS="1 2 4 8 16 32"
    export TRIALS=5
fi

if [ $(hostname) = "lenny" ]; # Lindsey's laptop
then
    export THREADS="1 2 4"
    export TRIALS=1
fi


make benchmark.run
./benchmark.run

if [ $(hostname) = "hive.soic.indiana.edu" ];
then
    unlock_for_experiments
fi

