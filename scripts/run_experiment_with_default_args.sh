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


SAMPLE=50000
SEED=0
RUNLABEL="default-args"

./scripts/run_jvm_opts.sh \
  --proteins $PROTFILE \
  --experiment $EXPFILE \
  --outlabel $RUNLABEL \
  --outfolder $OUTFOLDER \
  --arcsinh 5 \
  --alpha 0.5 \
  --neighbors 5 \
  --timeweight 0 \
  --iterations 100 \
  | tee -a log/${LOGLABEL}.log
