#!/bin/bash

echo "Running benchmarks remotely on server `hostname`"
set -e
set -x

# The working directory is passed as the first argument.
CHECKOUT=$1
cd "$CHECKOUT/"

pwd -P

# (1) Build everything
# ================================================================================

NOTEST=1 ./.jenkins_script.sh $*

# (2) Perform micro benchmarking
# ================================================================================
cd $CHECKOUT/haskell/lvish

HOST=`shell hostname -s`
REGRESS="--regress=allocated:iters --regress=bytesCopied:iters --regress=cycles:iters --regress=numGcs:iters \
    --regress=mutatorWallSeconds:iters --regress=gcWallSeconds:iters --regress=cpuTime:iters "

NAME=report_lvish_"$HOST"
OUTS= --raw "$NAME".criterion -o "$NAME".html

$CABAL bench --benchmark-options=" $WHICHBENCH --template=./report_format.tpl $REGRESS $OUTS +RTS -T -s -RTS"
# For performance debugging of benchmarks we include these:
# --ghc-options="-ddump-simpl -ddump-to-file"


# (3) Run larger benchmarks:
# ================================================================================

cd $CHECKOUT/haskell/lvish-apps

# TODO: FINISHME


# Old/scrap:
# ================================================================================
	
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

# echo "Printing out final .dat file:"
# cat result*.dat



