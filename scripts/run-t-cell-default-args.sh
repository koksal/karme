#!/bin/bash

OUTFOLDER=$1

./scripts/run-t-cell-base-args.sh $OUTFOLDER \
  --pseudolog-factor 2 \
  --boolean-normalization mclust \
  --cell-activity-threshold 0.2 \
  --uncertainty-threshold 1 \
  --smoothing-radius 15 \
  --cluster \
  --min-clusters 8 \
  --max-clusters 14 \
  --synthesis \
  --max-expr-depth 1
