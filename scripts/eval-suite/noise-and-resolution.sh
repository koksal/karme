#!/bin/bash

REPLICATES=5

if [ $# -eq 0 ]
then
  echo "Output folder not given."
  exit 1
else
  OUTFOLDER_BASE=$1
  shift
fi

DROP_PROBS=(0.95 0.99 0.992 0.993 0.994 0.995)
ERROR_PROBS=(0.001 0.002 0.003 0.004 0.005 0.01)

for replicate in `seq 1 $REPLICATES`
do
  for error_p in ${ERROR_PROBS[*]}
  do
    for drop_p in ${DROP_PROBS[*]}
    do
      ./scripts/run-synthetic-workflow.sh \
        $OUTFOLDER_BASE/noise-and-resolution/noise=$error_p-drop=$drop_p/replicate-$replicate \
        replicate
        --measurement-noise-prob $error_p \
        --random-seed $replicate \
        $*
    done
  done
done
