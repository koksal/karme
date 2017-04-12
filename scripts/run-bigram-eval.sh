#!/bin/bash

sbt "run-main karme.evaluation.BigramEvaluation \
  --bigrams bigrams-large.csv \
  --min-bigram-score 1666 \
  --name-universe data/names/experiment-genes.txt \
  --evaluation-libraries data/reference/enrichr/human-tf/ChEA_2016-enrichr-predictions.csv,data/reference/enrichr/human-tf/ENCODE_TF_ChIP-seq_2015-enrichr-predictions.csv,data/reference/enrichr/human-tf/ENCODE_and_ChEA_Consensus_TFs_from_ChIP-X-enrichr-predictions.csv,data/reference/enrichr/human-tf/TF-LOF_Expression_from_GEO-enrichr-predictions.csv,data/reference/enrichr/human-tf/TRANSFAC_and_JASPAR_PWMs-enrichr-predictions.csv \
  --max-library-predictions 3000 \
  --verbose"
