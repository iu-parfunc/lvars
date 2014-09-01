#!/bin/bash

# --------------
# DONT USE THIS.
# --------------
#
# This is for developers of the library and is here to deal with the
# temporary interdependencies between the latest versions of different
# lvish and monad-par packages.
#
# TESTING:  If you want to do testing, call this script with --enable-tests.
#   You may also find this useful:
#    find -name "*.log" | grep test | grep dist | xargs tail


D=`dirname $0`
M="$D/monad-par"

PKGS="$PKGS $D/lvish $D/par-classes/ $D/par-transformers/ $D/par-collections/"

# If you want to build monad-par HEAD, just check it out in this directory.
if [ -e "$D/monad-par" ] && [ "$MONADPAR" != 0 ]; then
  PKGS="$PKGS $M/monad-par $M/monad-par-extras/ $M/abstract-par/"
  echo ' [!!] Building monad-par as well as LVish.'
fi

set -e
set -x

## OPTIONS: These turn on the new, not-yet-official functionality
## within the packages in question.'
# ------------------------------------------------------------
# OPTS="-j -f-newgeneric"
OPTS=" -fnewgeneric -fgeneric"
# ------------------------------------------------------------

# When installing in a sandbox it makes thing a lot faster to skip
# this nonsense:

if [ -e ".cabal-sandbox" ]; then
  OPTS="$OPTS --disable-library-profiling --disable-documentation"
fi

# Finally, let's do this thing:
cabal install $OPTS $PKGS $*
