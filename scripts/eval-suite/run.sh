#!/bin/bash

if [ $# -eq 0 ]
then
  echo "Output folder not given."
  exit 1
else
  OUTFOLDER_BASE=$1
fi

EXTRA_ARGS="--distribution-comparison ranksum --distribution-comparison-p-value 0.05 --max-expr-depth 3"

# ./scripts/eval-suite/noise-and-resolution.sh $OUTFOLDER_BASE $EXTRA_ARGS
./scripts/eval-suite/partial-target-states.sh $OUTFOLDER_BASE $EXTRA_ARGS
