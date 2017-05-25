#!/bin/bash

OUTFOLDER=$1
shift

LOGFILE=$OUTFOLDER/log.txt

mkdir -p $OUTFOLDER

sbt "run-main karme.evaluation.Evaluation \
  --evaluation-libraries data/reference/t-cell-knockdown.csv,data/reference/t-cell-late-network.csv \
  --outfolder $OUTFOLDER \
  --verbose $*" | tee $LOGFILE
