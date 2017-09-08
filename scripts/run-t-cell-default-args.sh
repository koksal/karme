#!/bin/bash

OUTFOLDER_BASE=$1

TRAJECTORY_FILES=(
"pc1-values.csv"
"negative-pc1-values.csv"
"negative-pathogenicity-signature-large.csv"
"negative-pathogenicity-signature-manual.csv"
)

for DIST_COMP in "ranksum" "ks"
do
  for TRAJECTORY in "${TRAJECTORY_FILES[@]}"
  do
    TRAJECTORY_PATH=data/th17/trajectories/$TRAJECTORY

    OUTFOLDER=$OUTFOLDER_BASE/$TRAJECTORY-$DIST_COMP

    ./scripts/run-t-cell-base-args.sh $OUTFOLDER \
      --trajectories $TRAJECTORY_PATH \
      --distribution-comparison $DIST_COMP
  done
done
