#!/bin/bash

set -e
set -x

# This is specific to our testing setup at IU:
source $HOME/rn_jenkins_scripts/acquire_ghc.sh

cd haskell/lvish
cabal sandbox init
cabal sandbox hc-pkg list

if [ "$PROF" == "" ] || [ "$PROF" == "0" ]; then 
  CFG="--disable-library-profiling --disable-executable-profiling"
else
  CFG="--enable-library-profiling --enable-executable-profiling"
fi  
#   --reinstall  --force-reinstalls

cabal install $CFG $CABAL_FLAGS --only-dependencies --enable-tests
cabal configure $CFG $CABAL_FLAGS --with-ghc=ghc-$JENKINS_GHC

# Avoding the atomic-primops related bug on linux / GHC 7.6:
if [ `uname` == "Linux" ]; then
  cabal install
else
  cabal test --show-details=always
fi
