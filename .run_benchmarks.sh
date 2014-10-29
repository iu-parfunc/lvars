#!/bin/bash

echo "Running benchmarks remotely on server `hostname`"
set -e
set -x

# git submodule update

# The working directory is passed as the first argument.
CHECKOUT=$1
cd "$CHECKOUT/"

pwd -P
rm -rf pbbs/ 
git clone git@github.com:iu-parfunc/pbbs.git

cd "./haskell-prototype"


cabal install
cd apps
make fullrun

# NUMCPUS=`ls -d /sys/devices/system/cpu/cpu? /sys/devices/system/cpu/cpu?? 2> /dev/null | wc -l`
# if [ $NUMCPUS -lt 17 ]; then 
#   export THREADS=`seq 1 $NUMCPUS`
# else
#   THREADS="1 "
#   THREADS+=`seq 2 2 $NUMCPUS`
#   export THREADS
# fi


# CID=905767673358.apps.googleusercontent.com
# SEC=2a2H57dBggubW1_rqglC7jtK

# Enable upload of benchmarking data to a Google Fusion Table:
# hsbencher --fusion-upload --name monad-par-test --clientid=$CID --clientsecret=$SEC

echo "Printing out final .dat file:"
cat result*.dat
