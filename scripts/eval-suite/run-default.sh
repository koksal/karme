#!/bin/bash

if [ $# -eq 0 ]
then
  echo "Output folder not given."
  exit 1
else
  OUTFOLDER_BASE=$1
  shift
fi

./scripts/run-synthetic-workflow.sh \
  $OUTFOLDER_BASE/default \
  $*
