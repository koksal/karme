#!/bin/bash

OUTFOLDER=$1
PAIRFILE=$2

mkdir -p $OUTFOLDER

sbt "run-main karme.evaluation.IOPairEvaluation \
  --predictions $2
  --evaluation-libraries data/reference/t-cell-knockdown/cleaned-unique-abs-fold-changes.csv \
  --outfolder $OUTFOLDER \
  --verbose"
