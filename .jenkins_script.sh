#!/bin/bash

set -e
set -x

# This is specific to our testing setup at IU:
source $HOME/rn_jenkins_scripts/acquire_ghc.sh

cd haskell/lvish
cabal sandbox init
cabal sandbox hc-pkg list

MODE1="--enable-library-profiling --enable-executable-profiling"
MODE2="--disable-library-profiling --disable-executable-profiling"
CMDROOT="cabal install $CABAL_FLAGS --reinstall --with-ghc=ghc-$JENKINS_GHC --force-reinstalls"

# Avoding the atomic-primops related bug on linux / GHC 7.6:
if [ `uname` == "Linux" ]; then
  $CMDROOT $MODE2 
else
  $CMDROOT $MODE2 --enable-tests
fi
