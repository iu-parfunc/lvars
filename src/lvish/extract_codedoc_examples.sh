#!/bin/bash

rm -rf docs_examples
mkdir docs_examples

set -e

FILES="Control/LVish/DeepFrz.hs Control/LVish.hs"

for fl in $FILES; do 
  echo Extracting from $fl
  filename=`basename $fl`
  base="${filename%.*}"
  target="./docs_examples/$base.lhs"
  output="./docs_examples/$base.out"
  echo "  Base file name < $base >  It is your job to ensure these don't collide!"
  grep -E "^\w*> " $fl > $target
  echo "  Wrote `wc -l $target | awk '{ print $1 }'` lines of output to $target"
  echo "  Running with 'runghc', output in $output ..."
  runghc $target &> $output
  echo "  Done running, `wc -l $output | awk '{ print $1 }'` lines of output."
done

echo "Done with all extracted code examples."
