#!/bin/bash

if [ $# -eq 0 ]
then
  echo "Output folder not given."
  exit 1
else
  OUTFOLDER_BASE=$1
fi

EXTRA_ARGS="--partial-order comparison --distribution-comparison average --max-expr-depth 3"

# ./scripts/eval-suite/run-baseline.sh $OUTFOLDER_BASE $EXTRA_ARGS
./scripts/eval-suite/cell-trajectory-quality.sh $OUTFOLDER_BASE $EXTRA_ARGS
