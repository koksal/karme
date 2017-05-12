#!/bin/bash

OUTFOLDER=$1
LOGFILE=$OUTFOLDER/log.txt
shift

mkdir -p $OUTFOLDER

sbt "run-main karme.Main \
  --continuous-experiment data/measurements/t-cell-measurements.csv \
  --trajectories data/trajectories/t-cell-pc1-values.csv \
  --names data/names/t-cell-transcription-regulators.txt,data/names/t-cell-receptors.txt \
  --evaluation-libraries data/reference/enrichr/t-cell-names/ChEA_2016.csv,data/reference/enrichr/t-cell-names/ENCODE_TF_ChIP-seq_2015.csv,data/reference/enrichr/t-cell-names/ENCODE_and_ChEA_Consensus_TFs_from_ChIP-X.csv,data/reference/enrichr/t-cell-names/TF-LOF_Expression_from_GEO.csv,data/reference/enrichr/t-cell-names/TRANSFAC_and_JASPAR_PWMs.csv \
  --verbose \
  --outfolder $OUTFOLDER $*" | tee $LOGFILE
