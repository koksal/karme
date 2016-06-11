#!/bin/bash

TIMESTAMP=`date +%F-%H-%M-%S`
OUTFOLDER=./evaluation/${TIMESTAMP}
LOGLABEL=${TIMESTAMP}

EXPFILE=$1
PROTFILE=$2

if [ "$3" != "" ]; then
  LABEL=$3
  OUTFOLDER=${OUTFOLDER}-${LABEL}
  LOGLABEL=${LOGLABEL}-${LABEL}
fi

SAMPLE=20000
NBSEEDS=1

function run_with_seed() {
  SEED=$1
  RUNLABEL="seed-${SEED}"

  ./scripts/run_jvm_opts.sh \
    --proteins $PROTFILE \
    --experiment $EXPFILE \
    --outlabel $RUNLABEL \
    --outfolder $OUTFOLDER \
    --alpha 0.5 \
    --neighbors 5 \
    --timeweight 0.2 \
    --iterations 100 \
    --seed $SEED \
    | tee -a log/${LOGLABEL}.log
}

for seed in `seq 1 $NBSEEDS`
do
  run_with_seed $seed
done
