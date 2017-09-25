#!/bin/bash

if [ $# -eq 0 ]
then
  OUTFOLDER=`mktemp`
else
  OUTFOLDER=$1
fi

LOGFILE=$OUTFOLDER/log.txt
shift

mkdir -p $OUTFOLDER

sbt "run-main karme.evaluation.synthetic.Evaluation \
  --outfolder $OUTFOLDER $*" | tee $LOGFILE
