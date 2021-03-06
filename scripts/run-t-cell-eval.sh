#!/bin/bash

OUTFOLDER=$1
shift

LOGFILE=$OUTFOLDER/log.txt

mkdir -p $OUTFOLDER

sbt "run-main karme.evaluation.Evaluation \
  --evaluation-libraries data/reference/th17/knockdown-fold-changes.csv \
  --outfolder $OUTFOLDER \
  --verbose $*" | tee $LOGFILE
