#!/bin/bash

# A very simple regression test represinting the minimum standard for each checkin.
# You may need to run this script with "--reinstall" as an extra argument.

set -x
set -e

cabal install -f-beta -fdebug -fnewgeneric -fgeneric monad-par-0.3.4.6 \
   ./par-collections/ ./par-classes/ ./lvish/ ./par-transformers/ \
   --force-reinstalls  $*
#  ./\
#   ./lvish-apps/pbbs ./lvish-graph-algorithms/ \
