#!/bin/bash

# NOTE: uses env vars JENKINS_GHC and CABAL_FLAGS[1-3], if available.
#       Also passes through extra args to the major cabal install command.

set -e
set -x

# Now requiring a recent version of cabal:
if [ "$CABAL" == "" ]; then 
  CABAL=cabal-1.20
fi

SHOWDETAILS=streaming

if [ "$JENKINS_GHC" == "" ]; then 
  GHC=ghc
else
  ENVSCRIPT=$HOME/rn_jenkins_scripts/acquire_ghc.sh
  # This is specific to our testing setup at IU:
  if [ -f "$ENVSCRIPT" ]; then 
    source "$ENVSCRIPT"
  fi
  GHC=ghc-$JENKINS_GHC
fi

PKGS=" ./lvish ./par-classes ./par-collections ./par-transformers "

cd ./haskell/
TOP=`pwd`
$CABAL sandbox init
$CABAL sandbox hc-pkg list
for path in $PKGS; do 
  cd $TOP/$path
  $CABAL sandbox init --sandbox=$TOP/.cabal-sandbox
done
cd $TOP

# Always make sure the benchmarks build, even if we don't run them:
CFG=" --force-reinstalls --enable-benchmarks "

if [ "$PROF" == "" ] || [ "$PROF" == "0" ]; then 
  CFG="$CFG --disable-library-profiling --disable-executable-profiling"
else
  CFG="$CFG --enable-library-profiling --enable-executable-profiling"
fi  

if [ "$NOTEST" == "" ]; then 
  CFG="$CFG --enable-tests"
fi

# In newer cabal (>= 1.20) --enable-tests is separate from --run-tests:
$CABAL install $CFG $CABAL_FLAGS --with-ghc=$GHC $PKGS  $*

if [ "$NOTEST" == "" ]; then 
  for path in $PKGS; do 
    echo "Test package in path $path."
    cd $TOP/$path
    # Assume cabal 1.20+:
    echo "Do a reconfigure to make sure test doesn't rebuild with different arguments."
    $CABAL configure --with-ghc=$GHC --enable-tests $CABAL_FLAGS
    $CABAL test --show-details=$SHOWDETAILS --test-options='-j1 --jxml=test-results.xml --jxml-nested'
  done
fi
