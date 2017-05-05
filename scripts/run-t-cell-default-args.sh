#!/bin/bash

OUTFOLDER=$1

./scripts/run-t-cell-base-args.sh $OUTFOLDER \
  --pseudolog-factor 2 \
  --boolean-normalization mclust \
  --cell-activity-threshold 0.2 \
  --uncertainty-threshold 1 \
  --smoothing-radius 15 \
  --max-hamming 1 \
  --cluster \
  --min-clusters 10 \
  --max-clusters 10 \
  --synthesis
