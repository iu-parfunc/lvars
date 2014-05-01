#!/bin/bash

# NOTE: uses env vars JENKINS_GHC and CABAL_FLAGS, if available.
#       Also passes through extra args to the major cabal install command.

set -e
set -x

# Temporarily staying off of 1.20 due to cabal issue #1811:
CABAL=cabal-1.18.0
SHOWDETAILS=always
# SHOWDETAILS=streaming

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

PKGS=" ./lvish ./par-classes ./par-collections ./par-transformers"

cd ./haskell/
TOP=`pwd`
$CABAL sandbox init
$CABAL sandbox hc-pkg list
for path in $PKGS; do 
  cd $TOP/$path
  $CABAL sandbox init --sandbox=$TOP/.cabal-sandbox
done
cd $TOP

if [ "$PROF" == "" ] || [ "$PROF" == "0" ]; then 
  CFG="--disable-library-profiling --disable-executable-profiling"
else
  CFG="--enable-library-profiling --enable-executable-profiling"
fi  
#   --reinstall  --force-reinstalls

# Temporary hack, install containers first or we run into problems with the sandbox:
# $CABAL install containers --constraint='containers>=0.5.5.1'

# Also install custom version of monad-par:
$CABAL install $CFG $CABAL_FLAGS --with-ghc=$GHC $PKGS ./monad-par/monad-par/ --enable-tests --only-dep $*
$CABAL install $CFG $CABAL_FLAGS --with-ghc=$GHC $PKGS ./monad-par/monad-par/ $*

# Avoding the atomic-primops related bug on linux / GHC 7.6:
if ! [ `uname` == "Linux" ]; then  
  for path in $PKGS; do 
    echo "Test package in path $path."
    cd $TOP/$path
    # Assume cabal 1.20+:
    cabal test --show-details=$SHOWDETAILS
  done
fi
