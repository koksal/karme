#!/bin/bash
TIMESTAMP=`date +%F-%H-%M-%S`
OUTFOLDER=./evaluation/${TIMESTAMP}
if [ "$1" != "" ]; then
  LABEL=$1
  OUTFOLDER=${OUTFOLDER}-${LABEL}
fi

./scripts/evaluate.sh \
  --proteins data/simulated/names.txt \
  --simulate \
  --evaluate \
  --outfolder ${OUTFOLDER} \
  --sample 10000 \
  --arcsinh 5 \
  --alpha 0.5
