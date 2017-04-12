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
  --min-clusters 14 \
  --max-clusters 16 \
  --max-expr-depth 3 \
  --evaluation-libraries data/reference/enrichr/human-tf/ChEA_2016-enrichr-predictions.csv,data/reference/enrichr/human-tf/ENCODE_TF_ChIP-seq_2015-enrichr-predictions.csv,data/reference/enrichr/human-tf/ENCODE_and_ChEA_Consensus_TFs_from_ChIP-X-enrichr-predictions.csv,data/reference/enrichr/human-tf/TF-LOF_Expression_from_GEO-enrichr-predictions.csv,data/reference/enrichr/human-tf/TRANSFAC_and_JASPAR_PWMs-enrichr-predictions.csv
