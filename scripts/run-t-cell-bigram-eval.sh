#!/bin/bash

sbt "run-main karme.evaluation.BigramEvaluation \
  --bigrams t-cell-bigrams.csv \
  --evaluation-libraries data/reference/t-cell-knockdown/abs-fold-changes.csv \
  --verbose"
