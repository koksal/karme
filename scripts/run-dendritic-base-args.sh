#!/bin/bash

OUTFOLDER=$1
LOGFILE=$OUTFOLDER/log.txt
shift

mkdir -p $OUTFOLDER

sbt "run-main karme.Main \
  --continuous-experiment data/dendritic/measurements/measurements.csv \
  --names data/dendritic/names/knockdown-experiment-sources-and-targets.txt \
  --trajectories data/dendritic/trajectories/monocle-trajectory.csv \
  --knockdown-experiments data/reference/dendritic/knockdown/mmc2-nonzero-hits.csv,data/reference/dendritic/knockdown/mmc7-nonzero-hits.csv \
  --verbose \
  --outfolder $OUTFOLDER $*" | tee $LOGFILE
