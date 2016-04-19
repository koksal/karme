#!/bin/bash
TIMESTAMP=`date +%F-%H-%M-%S`
OUTFOLDER=./evaluation/${TIMESTAMP}
if [ "$1" != "" ]; then
  LABEL=$1
  OUTFOLDER=${OUTFOLDER}-${LABEL}
fi

readarray EXPERIMENTS < data/experiments.txt

for e in ${EXPERIMENTS[@]}
do
  echo Running experiment $e
  expFile=data/dremi/${e}.csv
  ./scripts/run.sh \
    --proteins data/proteins.txt \
    --experiment $expFile \
    --outlabel $e \
    --outfolder ${OUTFOLDER} \
    --sample 1000 \
    --seed 0 \
    --arcsinh 5 \
    --alpha 0.5 \
    --neighbors 5 \
    --timeweight 0 \
    --iterations 100
done
