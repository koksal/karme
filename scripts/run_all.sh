#!/bin/bash
TIMESTAMP=`date +%F-%H-%M-%S`
OUTFOLDER=./evaluation/${TIMESTAMP}
if [ "$1" != "" ]; then
  LABEL=$1
  OUTFOLDER=${OUTFOLDER}-${LABEL}
fi

readarray EXPERIMENTS < data/experiments-first.txt

for e in ${EXPERIMENTS[@]}
do
  echo Running experiment $e
  expFile=data/dremi/${e}.csv
  ./scripts/run.sh \
    --proteins data/proteins.txt \
    --experiment $expFile \
    --outfolder ${OUTFOLDER} \
    --outlabel $e \
    --sample \
    --seed 0 \
    --arcsinh 5 \
    --alpha 0.5 \
    --neighbors 5 \
    --iterations 10 \
    --split 5
done
