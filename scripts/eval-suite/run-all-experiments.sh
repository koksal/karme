#!/bin/bash

if [ $# -eq 0 ]
then
  echo "Output folder not given."
  exit 1
else
  OUTFOLDER_BASE=$1
fi

EXTRA_ARGS="--distribution-comparison ranksum --distribution-comparison-p-value 0.05 --max-expr-depth 2"

# ./scripts/eval-suite/cell-trajectory-quality.sh $OUTFOLDER_BASE $EXTRA_ARGS
./scripts/eval-suite/noise.sh $OUTFOLDER_BASE $EXTRA_ARGS
./scripts/eval-suite/resolution.sh $OUTFOLDER_BASE $EXTRA_ARGS
