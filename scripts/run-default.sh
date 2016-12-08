#!/bin/bash

OUTFOLDER=$1
LOGFILE=$OUTFOLDER/log.txt
shift

mkdir -p $OUTFOLDER

sbt "run --contexp data/continuous-experiment.csv --clusters data/clustering-renamed.csv --names data/markers.txt --trajectory data/curve1-pseudotime.csv,data/curve2-pseudotime.csv,data/curve3-pseudotime.csv --graphs --outfolder $OUTFOLDER $*" | tee $LOGFILE
