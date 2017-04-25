#!/bin/bash

OUTFOLDER=$1

./scripts/run-olfactory-base-args.sh $OUTFOLDER \
  --pseudolog-factor 2 \
  --boolean-normalization mclust \
  --cell-activity-threshold 0.2 \
  --uncertainty-threshold 0.4 \
  --smoothing-radius 20 \
  --max-hamming 1 \
  --cluster \
  --min-clusters 12 \
  --max-clusters 12
