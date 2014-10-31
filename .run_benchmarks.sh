#!/bin/bash

echo "Running benchmarks remotely on server `hostname`"
set -e
set -x

# The working directory is passed as the first argument.
CHECKOUT=$1
if [ "$CHECKOUT" == "" ]; then
    echo "ERROR: must pass working copy absolute path as first arg."
    exit 1
fi
cd "$CHECKOUT/"

pwd -P
# These are the arguments to the HSBencher harness...
export BENCHARGS=$*

if [ "$CABAL" == "" ]; then 
  CABAL=cabal-1.20
fi

# (1) Build everything
# ================================================================================

if [ "$MACHINECLASS" == cutter ]; then
    echo "WARNING: on cutter for some STRANGE reason, building in parallel with cabal crashes."
    echo "    It crashes with: 'libgcc_s.so.1 must be installed for pthread_cancel to work.'"
    echo "    Thus we build sequentially..."
    PARARG=""
else
    PARARG="-j"
fi

export EXTRAPKGS=" ./HSBencher/hsbencher ./HSBencher/hsbencher-fusion ./HSBencher/hsbencher-codespeed ./HSBencher/hgdata/ "
NOTEST=1 ./.jenkins_script.sh $PARARG

# (2) Perform micro benchmarking
# ================================================================================
cd $CHECKOUT/haskell/lvish

HOST=`hostname -s`
REGRESS="--regress=allocated:iters --regress=bytesCopied:iters --regress=cycles:iters --regress=numGcs:iters
      --regress=mutatorWallSeconds:iters --regress=gcWallSeconds:iters --regress=cpuTime:iters "

NAME="report_lvish_$HOST"
OUTS=" --raw $NAME.criterion -o $NAME.html"

# Need to reconfigure because of cabal issue #2182:
$CABAL configure --enable-benchmarks
$CABAL bench --benchmark-options=" $WHICHBENCH --template=./report_format.tpl $REGRESS $OUTS +RTS -T -s -RTS"
# For performance debugging of benchmarks we include these:
# --ghc-options="-ddump-simpl -ddump-to-file"

# Use the LVish uploader project
CID=820162629229-kp29aklebt6ucos5a71u8tu3hu8unres.apps.googleusercontent.com
SEC=pSsMxVAJCFKyWsazuxZVRZwX
# CID=905767673358.apps.googleusercontent.com
# SEC=2a2H57dBggubW1_rqglC7jtK

export HSBENCHER_GOOGLE_CLIENTID=$CID
export HSBENCHER_GOOGLE_CLIENTSECRET=$SEC
cabal exec fusion-upload-criterion -- --name=LVish_microbench_results $NAME.criterion
# cabal exec fusion-upload-criterion -- --name=LVish_microbench_results --clientid=$CID --clientsecret=$SEC $NAME.criterion

# Phone home and send the report
function phone_home() {
    STAMP=`date +"%Y_%M_%d_%H:%M:%S"`
    mkdir -p $HOME/collected_criterion_reports 
    # Copy locally first:
    cp $NAME.html $HOME/collected_criterion_reports/"$STAMP"_$NAME.html    
    ssh parfunc@tank.soic.indiana.edu mkdir -p collected_criterion_reports 
    scp $NAME.html parfunc@tank.soic.indiana.edu:collected_criterion_reports/"$STAMP"_$NAME.html 
}

phone_home || echo "ok if this fails."


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


# Enable upload of benchmarking data to a Google Fusion Table:
# hsbencher --fusion-upload --name monad-par-test --clientid=$CID --clientsecret=$SEC

# echo "Printing out final .dat file:"
# cat result*.dat



