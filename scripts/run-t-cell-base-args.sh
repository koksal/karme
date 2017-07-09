#!/bin/bash

OUTFOLDER=$1
LOGFILE=$OUTFOLDER/log.txt
shift

mkdir -p $OUTFOLDER

sbt "run-main karme.Main \
  --continuous-experiment data/th17/measurements/measurements.csv \
  --trajectories data/th17/trajectories/pc1-values.csv \
  --names data/th17/names/transcription-regulators.txt,data/th17/names/receptors.txt \
  --evaluation-libraries data/reference/th17/knockdown-binary-effects.csv \
  --verbose \
  --outfolder $OUTFOLDER $*" | tee $LOGFILE
