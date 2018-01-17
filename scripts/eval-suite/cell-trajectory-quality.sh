#!/bin/bash

if [ $# -eq 0 ]
then
  echo "Output folder not given."
  exit 1
else
  OUTFOLDER_BASE=$1
  shift
fi

for sigma in 0 0.1 0.2 0.5 1 2
do
  ./scripts/run-synthetic-workflow.sh \
    $OUTFOLDER_BASE/cell-trajectory-quality/sigma=$sigma \
    --cell-trajectory-noise-sigma $sigma \
    $*
done
