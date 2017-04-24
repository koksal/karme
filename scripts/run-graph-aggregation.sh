#!/bin/bash

OUTFOLDER=$1
LOGFILE=$OUTFOLDER/log.txt
shift

mkdir -p $OUTFOLDER

sbt "run-main karme.GraphAggregation \
  --outfolder $OUTFOLDER $*" | tee $LOGFILE
