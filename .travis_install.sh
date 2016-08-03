#!/bin/bash

# Assumes that "stack" is in the path:

set -xe

# In this mode we just grab the latest from hackage:
if [ ${STACK_RESOLVER%-*} = "default" ]; then
    mv stack.yaml stack-${STACK_RESOLVER}.yaml
    which -a ghc
    ghc --version
    # stack --resolver=${STACK_RESOLVER} solver --modify-stack-yaml
else
    cat stack.yaml | grep -v resolver > stack-${STACK_RESOLVER}.yaml
    echo "resolver: ${STACK_RESOLVER}" >> stack-${STACK_RESOLVER}.yaml
    rm -f stack.yaml # Just to make sure.
fi

cat stack-${STACK_RESOLVER}.yaml
# Sweet and simple.  Install upstream dependencies, including GHC:
stack setup --no-terminal
stack build
stack test --only-snapshot --no-terminal
