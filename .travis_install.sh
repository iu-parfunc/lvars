#!/bin/bash

# Assumes that "stack" is in the path:

set -xe

cat stack.yaml | grep -v resolver > stack-${STACK_RESOLVER}.yaml
echo "resolver: ${STACK_RESOLVER}" >> stack-${STACK_RESOLVER}.yaml
rm -f stack.yaml # Just to make sure.

# In this mode we just grab the latest from hackage:
if [ ${STACK_RESOLVER%-*} = "default" ]; then
    which -a ghc
    ghc --version
    stack --resolver=${STACK_RESOLVER} solver --modify-stack-yaml
fi

# Sweet and simple.  Install upstream dependencies, including GHC:
stack setup --no-terminal
stack test --only-snapshot --no-terminal
