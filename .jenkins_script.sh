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

# TEMP: Disable unit tests for now.
# There are some serious problems with the testing setup.  
# Including the atomic-primops related bug on linux, as well as other problems.

$CMDROOT $MODE2 --enable-tests
