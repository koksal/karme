#!/bin/bash

OUTFOLDER_BASE=$1

TRAJECTORY_FILES=(
  "pc1-values.csv"
  "negative-pathogenicity-signature-large.csv"
  "negative-pathogenicity-signature-manual.csv"
  )

for TRAJECTORY in "${TRAJECTORY_FILES[@]}"
do
  TRAJECTORY_PATH=data/th17/trajectories/$TRAJECTORY

  for DIFF_RATIO in 0.1 0.2
  do

    for RADIUS in 10 20 40
    do

      for CLUST_METHOD in "complete" "average" "kmeans"
      do

        OUTFOLDER=$OUTFOLDER_BASE-$TRAJECTORY-diff-ratio-$DIFF_RATIO-radius-$RADIUS-clustering-$CLUST_METHOD

        ./scripts/run-t-cell-base-args.sh $OUTFOLDER \
          --trajectories $TRAJECTORY_PATH \
          --cell-activity-threshold 0.1 \
          --uncertainty-threshold 1 \
          --smoothing-radius $RADIUS \
          --clustering-method $CLUST_METHOD \
          --clustering-index kl \
          --plot-binarized-data \
          --plot-binarized-curves \
          --plot-smoothed-curves
      done
    done
  done
done
