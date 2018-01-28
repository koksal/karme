#!/bin/bash

REPLICATES=1

if [ $# -eq 0 ]
then
  echo "Output folder not given."
  exit 1
else
  OUTFOLDER_BASE=$1
  shift
fi

DROP_PROBS=(0 0.5 0.99)

for replicate in `seq 1 $REPLICATES`
do
  for drop_p in ${DROP_PROBS[*]}
  do
    ./scripts/run-synthetic-workflow.sh \
      $OUTFOLDER_BASE/resolution/p=$drop_p/replicate-$replicate \
      --measurement-drop-prob $drop_p \
      --random-seed $replicate \
      $*
  done
done
