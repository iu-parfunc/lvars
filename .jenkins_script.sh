#!/bin/bash

# NOTE: Passes through extra args to the major cabal install command.
#       Also uses these environment vars, if available:
#        * JENKINS_GHC
#        * STACK_FLAGS
#        * NOTEST
#        * CABAL
#        * EXTRAPKGS -- useful for including packages in the one-big-install

set -e
set -x

SHOWDETAILS=streaming

# Just use which stack is in scope:
STACK=stack
# STACK=stack-1.0.4.2
which -a $STACK

DISABLE_EXEC_PROF="--disable-profiling"
ENABLE_EXEC_PROF="--enable-profiling"
# Old version, GHC-7.8:
# DISABLE_EXEC_PROF="--disable-executable-profiling"
# ENABLE_EXEC_PROF="--enable-executable-profiling"

# Always make sure the benchmarks build, even if we don't run them:
CFG=" --bench "

for flg in $STACK_FLAGS; do
  CFG+=" --flags=*:${flg} "
done

if [ "$PROF" == "" ] || [ "$PROF" == "0" ]; then
  CFG="$CFG --disable-library-profiling $DISABLE_EXEC_PROF"
else
  CFG="$CFG --enable-library-profiling $ENABLE_EXEC_PROF"
fi

if [ "$NOTEST" == "" ]; then
  CFG="$CFG --enable-tests"
fi

echo "Running stack version "`$STACK --version`" with options: $CFG"

stack --no-system-ghc --install-ghc test $CFG
