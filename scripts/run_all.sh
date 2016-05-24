#!/bin/bash
TIMESTAMP=`date +%F-%H-%M-%S`
OUTFOLDER=./evaluation/${TIMESTAMP}
LOGLABEL=${TIMESTAMP}

EXPLISTFILE=data/experiments.txt
PROTFILE=data/proteins.txt

if [ "$1" != "" ]; then
  LABEL=$1
  OUTFOLDER=${OUTFOLDER}-${LABEL}
  LOGLABEL=${LOGLABEL}-${LABEL}
fi

EXPERIMENTS=()
while read -r line
do
  EXPERIMENTS+=($line)
done < $EXPLISTFILE

for e in ${EXPERIMENTS[@]}
do
  echo Running experiment $e
  for seed in 0 1 2
  do
    RUNLABEL=$e-seed-$seed
    EXPFILE=data/dremi/${e}.csv
    ./scripts/run_jvm_opts.sh \
      --proteins $PROTFILE \
      --experiment $EXPFILE \
      --outlabel $RUNLABEL \
      --outfolder ${OUTFOLDER} \
      # --sample 50000 \
      --seed $seed \
      --arcsinh 5 \
      --alpha 0.5 \
      --neighbors 5 \
      --timeweight 0 \
      --iterations 100 \
      | tee -a log/${LOGLABEL}.log
  done
done
