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

for replicate in `seq 1 $REPLICATES`
do
  for error_p in 0 0.005 0.01
  do
    ./scripts/run-synthetic-workflow.sh \
      $OUTFOLDER_BASE/noise/p=$error_p/replicate-$replicate \
      --measurement-noise-prob $error_p \
      --random-seed $replicate \
      $*
  done
done
