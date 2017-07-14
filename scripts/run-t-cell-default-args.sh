#!/bin/bash

OUTFOLDER_BASE=$1

TRAJECTORY_FILES=(
  # "pc1-values.csv"
  "negative-pathogenicity-signature-large.csv"
  # "negative-pathogenicity-signature-manual.csv"
  )

for TRAJECTORY in "${TRAJECTORY_FILES[@]}"
do
  TRAJECTORY_PATH=data/th17/trajectories/$TRAJECTORY

  for DIFF_RATIO in 0.1
  do

    for RADIUS in 40
    do

      for CLUST_METHOD in "kmeans"
      do

        for k in {10..20}
        do

          OUTFOLDER=$OUTFOLDER_BASE-$TRAJECTORY-diff-ratio-$DIFF_RATIO-radius-$RADIUS-clustering-$CLUST_METHOD-$k

          ./scripts/run-t-cell-base-args.sh $OUTFOLDER \
            --trajectories $TRAJECTORY_PATH \
            --cell-activity-threshold $DIFF_RATIO \
            --smoothing-radius $RADIUS \
            --clustering-method $CLUST_METHOD \
            --clustering-index kl \
            --min-clusters $k \
            --max-clusters $k \
            --refine-clusters \
            --cluster-refinement-p-value 0.05 \
            --synthesis \
            --max-expr-depth 1
        done
      done
    done
  done
done
