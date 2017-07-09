#!/bin/bash

OUTFOLDER=$1

for radius in 5 10 20 40
do
  ./scripts/run-t-cell-base-args.sh $OUTFOLDER-smoothing-radius-$radius \
    --cell-activity-threshold 0.1 \
    --plot-binarized-curves \
    --plot-smoothed-curves \
    --uncertainty-threshold 1 \
    --smoothing-radius $radius
done
