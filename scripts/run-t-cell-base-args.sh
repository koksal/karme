#!/bin/bash

OUTFOLDER=$1
LOGFILE=$OUTFOLDER/log.txt
shift

mkdir -p $OUTFOLDER

sbt "run-main karme.Main \
  --continuous-experiment data/th17/measurements/measurements.csv \
  --names data/th17/names/knockdown-sources.txt,data/th17/names/selected-knockdown-targets.txt \
  --evaluation-libraries data/reference/th17/knockdown-binary-effects.csv \
  --verbose \
  --outfolder $OUTFOLDER $*" | tee $LOGFILE
