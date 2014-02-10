#!/bin/bash

# A very simple regression test represinting the minimum standard for each checkin.
# You may need to run this script with "--reinstall" as an extra argument.

set -x
set -e

# cabal install -fnewgeneric -fgeneric ./monad-par/monad-par/ ./par-collections/ ./par-classes/ ./par-transformers/ ./lvish/ --enable-tests --force-reinstalls

cabal install -fdebug -fnewgeneric -fgeneric monad-par-0.3.4.6 \
   ./par-collections/ ./par-classes/ ./par-transformers/ ./lvish/ \
   --force-reinstalls --disable-documentation  $*

# Temporarily disabled while porting over to LVish 2.0:
#   ./lvish-apps/pbbs ./lvish-graph-algorithms/ \

# TODO, add:
#   lvish-apps/cfa
