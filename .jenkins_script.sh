#!/bin/bash

# NOTE: Passes through extra args to the major cabal install command.
#       Also uses these environment vars, if available:
#        * JENKINS_GHC
#        * CABAL_FLAGS[1-3]
#        * NOTEST
#        * CABAL
#        * EXTRAPKGS -- useful for including packages in the one-big-install

set -e
set -x

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

# Now requiring a recent version of cabal:
if [ "$CABAL" == "" ]; then
  if [ "$GHC" == "ghc-7.10.1" ]; then
      CABAL=cabal-1.22
      DISABLE_EXEC_PROF="--disable-profiling"
      ENABLE_EXEC_PROF="--enable-profiling"
  else
      CABAL=cabal-1.20
      DISABLE_EXEC_PROF="--disable-executable-profiling"
      ENABLE_EXEC_PROF="--enable-executable-profiling"
  fi
fi

PKGS=" ./lvish ./par-classes ./par-collections ./par-transformers ./concurrent-skiplist ./par-mergesort "

# We build the sandbox, not at the repo root, but at the root of the Haskell code dir.
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
  CFG="$CFG --disable-library-profiling $DISABLE_EXEC_PROF"
else
  CFG="$CFG --enable-library-profiling $ENABLE_EXEC_PROF"
fi

if [ "$NOTEST" == "" ]; then
  CFG="$CFG --enable-tests"
fi

# In newer cabal (>= 1.20) --enable-tests is separate from --run-tests:
$CABAL install $CFG $CABAL_FLAGS --with-ghc=$GHC -fdebug $PKGS $EXTRAPKGS $*

if [ "$NOTEST" == "" ]; then
  for path in $PKGS; do
    echo "Test package in path $path."
    cd $TOP/$path
    # Assume cabal 1.20+:
    echo "Do a reconfigure to make sure test doesn't rebuild with different arguments."
    $CABAL configure --with-ghc=$GHC -fdebug --enable-tests $CABAL_FLAGS
    $CABAL test --show-details=$SHOWDETAILS --test-options='-j1 --jxml=test-results.xml --jxml-nested'
  done
fi
