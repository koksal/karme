#!/bin/bash

REPLICATES=3

if [ $# -eq 0 ]
then
  echo "Output folder not given."
  exit 1
else
  OUTFOLDER_BASE=$1
  shift
fi

SIGMAS=(0 0.1 0.2 0.5 1 2)

for replicate in `seq 1 $REPLICATES`
do
  for sigma in ${SIGMAS[*]}
  do
    ./scripts/run-synthetic-workflow.sh \
      $OUTFOLDER_BASE/cell-trajectory-quality/sigma=$sigma/replicate-$replicate \
      --cell-trajectory-noise-sigma $sigma \
      --random-seed $replicate \
      $*
  done
done