#!/bin/bash

OUTFOLDER=$1
LOGFILE=$OUTFOLDER/log.txt
shift

mkdir -p $OUTFOLDER

sbt "run-main karme.Main \
  --continuous-experiment data/th17/measurements/measurements.csv \
  --names data/th17/names/knockdown-sources.txt,data/th17/names/knockdown-targets.txt \
  --knockdown-experiments data/reference/th17/knockdown-fold-changes.csv \
  --verbose \
  --outfolder $OUTFOLDER $*" | tee $LOGFILE
