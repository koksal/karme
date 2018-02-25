#!/bin/bash

if [ $# -eq 0 ]
then
  OUTFOLDER=`mktemp -d ./temp-run-folder.XXX`
else
  OUTFOLDER=$1
  mkdir -p $OUTFOLDER
fi

echo "Output folder: " $OUTFOLDER

LOGFILE=$OUTFOLDER/log.txt
shift

EXTRA_ARGS="--distribution-comparison ranksum --distribution-comparison-p-value 0.05 --max-expr-depth 3"

sbt "run-main karme.evaluation.synthetic.Workflow \
  --verbose \
  --outfolder $OUTFOLDER $EXTRA_ARGS $*" | tee $LOGFILE
