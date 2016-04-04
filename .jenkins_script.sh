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

# Always make sure the benchmarks build, even if we don't run them:
CFG=" --bench "

for flg in $STACK_FLAGS; do
  CFG+=" --flag=*:${flg} "
done

if [ "$PROF" == "" ] || [ "$PROF" == "0" ]; then
  CFG="$CFG --no-executable-profiling --no-library-profiling $DISABLE_EXEC_PROF"
else
  CFG="$CFG --executable-profiling --library-profiling $ENABLE_EXEC_PROF"
fi

if [ "$NOTEST" == "" ]; then
  CFG="$CFG --enable-tests"
fi

echo "Running stack version "`$STACK --version`" with options: $CFG"

stack --no-system-ghc --install-ghc test $CFG
