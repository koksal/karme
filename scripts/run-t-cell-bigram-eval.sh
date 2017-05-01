#!/bin/bash

PAIRFILE=$1

sbt "run-main karme.evaluation.BigramEvaluation \
  --bigrams $PAIRFILE \
  --evaluation-libraries data/reference/enrichr/t-cell-names/ChEA_2016.csv,data/reference/enrichr/t-cell-names/ENCODE_TF_ChIP-seq_2015.csv,data/reference/enrichr/t-cell-names/ENCODE_and_ChEA_Consensus_TFs_from_ChIP-X.csv,data/reference/enrichr/t-cell-names/TF-LOF_Expression_from_GEO.csv,data/reference/enrichr/t-cell-names/TRANSFAC_and_JASPAR_PWMs.csv,data/reference/t-cell-knockdown/abs-fold-changes.csv \
  --verbose"
