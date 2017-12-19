#!/bin/bash

if [ $# -eq 0 ]
then
  echo "Output folder not given."
  exit 1
else
  OUTFOLDER_BASE=$1
fi

EXTRA_ARGS="--partial-order comparison --distribution-comparison average --max-expr-depth 3"

./scripts/eval-suite/run-default.sh $OUTFOLDER_BASE $EXTRA_ARGS
