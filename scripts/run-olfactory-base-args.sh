#!/bin/bash

OUTFOLDER=$1
LOGFILE=$OUTFOLDER/log.txt
shift

mkdir -p $OUTFOLDER

sbt "run-main karme.GraphAggregation \
  --continuous-experiment data/measurements/continuous-experiment.csv \
  --trajectories data/trajectories/curve1-pseudotime.csv,data/trajectories/curve2-pseudotime.csv,data/trajectories/curve3-pseudotime.csv \
  --names data/names/human-tf.txt \
  --annotations data/names/markers.txt \
  --cell-clusters data/cell-clustering/clustering-renamed.csv \
  --evaluation-libraries data/reference/enrichr/human-tf/ChEA_2016-enrichr-predictions.csv,data/reference/enrichr/human-tf/ENCODE_TF_ChIP-seq_2015-enrichr-predictions.csv,data/reference/enrichr/human-tf/ENCODE_and_ChEA_Consensus_TFs_from_ChIP-X-enrichr-predictions.csv,data/reference/enrichr/human-tf/TF-LOF_Expression_from_GEO-enrichr-predictions.csv,data/reference/enrichr/human-tf/TRANSFAC_and_JASPAR_PWMs-enrichr-predictions.csv \
  --verbose \
  --outfolder $OUTFOLDER $*" | tee $LOGFILE
