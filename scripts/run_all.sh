#!/bin/bash
TIMESTAMP=`date +%F-%H-%M-%S`
OUTFOLDER=./evaluation/${TIMESTAMP}
if [ "$1" != "" ]; then
  LABEL=$1
  OUTFOLDER=${OUTFOLDER}-${LABEL}
fi

EXPERIMENTS=()
while read -r line
do
  EXPERIMENTS+=($line)
done < data/large-experiment.txt

for e in ${EXPERIMENTS[@]}
do
  echo Running experiment $e
  for seed in 0
  do
    label=$e-seed-$seed
    expFile=data/dremi/${e}.csv
    ./scripts/run_jvm_opts.sh \
      --proteins data/proteins.txt \
      --experiment $expFile \
      --outlabel $label \
      --outfolder ${OUTFOLDER} \
      --sample 2000 \
      --seed $seed \
      --arcsinh 5 \
      --alpha 0.5 \
      --neighbors 5 \
      --timeweight 0 \
      --iterations 100 \
      | tee log/${TIMESTAMP}.log
  done
done
