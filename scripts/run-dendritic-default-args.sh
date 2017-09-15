#!/bin/bash

OUTFOLDER_BASE=$1

for DIST_COMP in "ranksum"
do
  for P_VAL in 0.005 0.01 0.05 0.1 0.2
  do
    FOLDER=$OUTFOLDER_BASE/$DIST_COMP-$P_VAL
    ./scripts/run-dendritic-base-args.sh $FOLDER \
      --distribution-comparison $DIST_COMP \
      --distribution-comparison-p-value $P_VAL
  done
done
