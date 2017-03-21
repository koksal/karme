#!/bin/bash

OUTFOLDER=$1

./scripts/run-with-base-args.sh $OUTFOLDER \
  --names data/names/human-tf.txt \
  --annotations data/names/markers.txt \
  --synthesis \
  --pseudolog-factor 2 \
  --boolean-normalization mclust \
  --cell-activity-threshold 0.2 \
  --uncertainty-threshold 0.4 \
  --smoothing-radius 20 \
  --max-hamming 1 \
  --cluster \
  --min-clusters 10 \
  --max-clusters 20 \
  --max-expr-depth 5 \
  --evaluation-libraries data/reference/enrichr/human-tf/ENCODE_and_ChEA_Consensus_TFs_from_ChIP-X-enrichr-predictions.csv
