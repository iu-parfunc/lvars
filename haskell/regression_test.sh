#!/bin/bash

set -x
set -e

# cabal install -fnewgeneric -fgeneric ./monad-par/monad-par/ ./par-collections/ ./par-classes/ ./par-transformers/ ./lvish/ --enable-tests --force-reinstalls

cabal install -fdebug -fnewgeneric -fgeneric monad-par-0.3.4.6 \
   ./par-collections/ ./par-classes/ ./par-transformers/ ./lvish/ ./lvish-graph-algorithms/ \
   --enable-tests --force-reinstalls -j $*

