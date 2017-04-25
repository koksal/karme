#!/bin/bash

OUTFOLDER=$1
LOGFILE=$OUTFOLDER/log.txt
shift

mkdir -p $OUTFOLDER

sbt "run-main karme.GraphAggregation \
  --continuous-experiment data/measurements/continuous-experiment.csv \
  --cell-clusters data/cell-clustering/clustering-renamed.csv \
  --trajectories data/trajectories/curve1-pseudotime.csv,data/trajectories/curve2-pseudotime.csv,data/trajectories/curve3-pseudotime.csv \
  --verbose \
  --outfolder $OUTFOLDER $*" | tee $LOGFILE
