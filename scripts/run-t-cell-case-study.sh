#!/bin/bash

OUTFOLDER="log/t-cell-run"
LOGFILE=$OUTFOLDER/log.txt
mkdir -p $OUTFOLDER

sbt "run-main karme.Main \
  --continuous-experiment data/measurement/t-cell-measurements.csv \
  --trajectories data/trajectories/t-cell-pc1-values.csv \
  --names data/names/t-cell-transcription-regulators.txt \
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
  --max-expr-depth 3 \
  --evaluation-libraries data/reference/enrichr/human-tf/ChEA_2016-enrichr-predictions.csv,data/reference/enrichr/human-tf/ENCODE_TF_ChIP-seq_2015-enrichr-predictions.csv,data/reference/enrichr/human-tf/ENCODE_and_ChEA_Consensus_TFs_from_ChIP-X-enrichr-predictions.csv,data/reference/enrichr/human-tf/TF-LOF_Expression_from_GEO-enrichr-predictions.csv,data/reference/enrichr/human-tf/TRANSFAC_and_JASPAR_PWMs-enrichr-predictions.csv \
  --verbose \
  --outfolder $OUTFOLDER" | tee $LOGFILE

