#!/bin/bash

OUTFOLDER=$1
LOGFILE=$OUTFOLDER/log.txt
shift

mkdir -p $OUTFOLDER

sbt "run-main karme.Main \
  --contexp data/measurements/continuous-experiment.csv \
  --cell-clusters data/cell-clustering/clustering-renamed.csv \
  --trajectory data/trajectories/curve1-pseudotime.csv,data/trajectories/curve2-pseudotime.csv,data/trajectories/curve3-pseudotime.csv \
  --outfolder $OUTFOLDER $*" | tee $LOGFILE
