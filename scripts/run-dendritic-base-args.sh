#!/bin/bash

OUTFOLDER=$1
LOGFILE=$OUTFOLDER/log.txt
shift

mkdir -p $OUTFOLDER

sbt "run-main karme.Main \
  --continuous-experiment data/dendritic/measurements/measurements.csv \
  --names data/dendritic/names/expressed-genes.txt \
  --trajectories data/dendritic/trajectories/monocle-trajectory.csv \
  --verbose \
  --outfolder $OUTFOLDER $*" | tee $LOGFILE
