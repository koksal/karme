#!/bin/bash

if [ $# -eq 0 ]
then
  echo "Output folder not given."
  exit 1
else
  OUTFOLDER_BASE=$1
  shift
fi

for replicate in `seq 2 5`
do
  for sigma in 0.1 0.2 0.5 1 2
  do
    ./scripts/run-synthetic-workflow.sh \
      $OUTFOLDER_BASE/cell-trajectory-quality/sigma=$sigma/replicate-$replicate \
      --cell-trajectory-noise-sigma $sigma \
      --random-seed $replicate \
      $*
  done
done
