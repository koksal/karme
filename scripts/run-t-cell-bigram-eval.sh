#!/bin/bash

OUTFOLDER=$1
PAIRFILE=$2

mkdir -p $OUTFOLDER

sbt "run-main karme.evaluation.IOPairEvaluation \
  --predictions $2
  --evaluation-libraries data/reference/enrichr/t-cell-names/ChEA_2016.csv,data/reference/enrichr/t-cell-names/ENCODE_TF_ChIP-seq_2015.csv,data/reference/enrichr/t-cell-names/ENCODE_and_ChEA_Consensus_TFs_from_ChIP-X.csv,data/reference/enrichr/t-cell-names/TF-LOF_Expression_from_GEO.csv,data/reference/enrichr/t-cell-names/TRANSFAC_and_JASPAR_PWMs.csv,data/reference/t-cell-knockdown/abs-fold-changes.csv \
  --outfolder $OUTFOLDER \
  --verbose"
