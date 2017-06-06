#!/bin/bash

OUTFOLDER=$1

./scripts/run-olfactory-base-args.sh $OUTFOLDER \
  --pseudolog-factor 10 \
  --boolean-normalization mclust \
  --cell-activity-threshold 0.2 \
  --uncertainty-threshold 1 \
  --smoothing-radius 20 \
  --cluster \
  --min-clusters 10 \
  --max-clusters 10
