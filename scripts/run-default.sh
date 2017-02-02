#!/bin/bash

OUTFOLDER=$1
LOGFILE=$OUTFOLDER/log.txt
shift

mkdir -p $OUTFOLDER

sbt "run-main karme.Main --contexp data/continuous-experiment.csv --nbclusters 30 --clusters data/clustering-renamed.csv --names data/markers-and-tf-in-experiment.txt --trajectory data/curve1-pseudotime.csv,data/curve2-pseudotime.csv,data/curve3-pseudotime.csv --graphs --outfolder $OUTFOLDER $*" | tee $LOGFILE
