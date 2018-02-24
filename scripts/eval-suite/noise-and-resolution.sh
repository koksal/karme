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

TPR_RANGE=(0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1)
FDR_RANGE=(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8)

for replicate in `seq 1 $REPLICATES`
do
  for TPR in ${TPR_RANGE[*]}
  do
    for FDR in ${FDR_RANGE[*]}
    do
      ./scripts/run-synthetic-workflow.sh \
        $OUTFOLDER_BASE/noise-and-resolution/TPR=$TPR-FDR=$FDR/replicate-$replicate \
        --state-tpr $TPR \
        --state-fdr $FDR \
        --random-seed $replicate \
        $*
    done
  done
done
