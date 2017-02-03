#!/bin/bash

OUTFOLDER=$1
LOGFILE=$OUTFOLDER/log.txt
shift

mkdir -p $OUTFOLDER

sbt "run-main karme.Main \
  --contexp data/continuous-experiment.csv \
  --clusters data/clustering-renamed.csv \
  --trajectory data/curve1-pseudotime.csv,data/curve2-pseudotime.csv,data/curve3-pseudotime.csv \
  --outfolder $OUTFOLDER $*" | tee $LOGFILE
