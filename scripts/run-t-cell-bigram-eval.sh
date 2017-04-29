#!/bin/bash

PAIRFILE=$1

sbt "run-main karme.evaluation.BigramEvaluation \
  --bigrams $PAIRFILE \
  --evaluation-libraries data/reference/t-cell-knockdown/abs-fold-changes.csv \
  --verbose"
