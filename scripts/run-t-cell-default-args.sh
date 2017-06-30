#!/bin/bash

OUTFOLDER=$1

./scripts/run-t-cell-base-args.sh $OUTFOLDER \
  --pseudolog-factor 10 \
  --boolean-normalization mclust \
  --plot-binarized-data \
  --cell-activity-threshold 0.2 \
  --uncertainty-threshold 1 \
  --smoothing-radius 20 \
  --cluster \
  --min-clusters 7 \
  --max-clusters 7 \
  --refine-clusters \
  --cluster-refinement-p-value 0.01 \
  --synthesis \
  --max-expr-depth 1
