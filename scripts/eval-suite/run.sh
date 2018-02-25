#!/bin/bash

if [ $# -eq 0 ]
then
  echo "Output folder not given."
  exit 1
else
  OUTFOLDER_BASE=$1
fi

./scripts/eval-suite/noise-and-resolution.sh $OUTFOLDER_BASE
