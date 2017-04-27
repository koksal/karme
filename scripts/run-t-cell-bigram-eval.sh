#!/bin/bash

sbt "run-main karme.evaluation.BigramEvaluation \
  --bigrams t-cell-within-cluster-counts.csv \
  --evaluation-libraries data/reference/t-cell-knockdown/abs-fold-changes.csv \
  --verbose"
