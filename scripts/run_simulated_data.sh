#!/bin/bash
TIMESTAMP=`date +%F-%H-%M-%S`
OUTFOLDER=./evaluation/${TIMESTAMP}
if [ "$1" != "" ]; then
  LABEL=$1
  OUTFOLDER=${OUTFOLDER}-${LABEL}
fi

./scripts/run.sh \
  --proteins data/proteins.txt \
  --simulate \
  --outfolder ${OUTFOLDER} \
  --sample 10000 \
  --seed 0 \
  --arcsinh 5 \
  --alpha 0.5 \
  --neighbors 5 \
  --timeweight 0.5 \
  --iterations 10
