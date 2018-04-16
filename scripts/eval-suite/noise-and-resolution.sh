#!/bin/bash

# REPLICATES=5
REPLICATES=1

if [ $# -eq 0 ]
then
  echo "Output folder not given."
  exit 1
else
  OUTFOLDER_BASE=$1
  shift
fi

sbt stage
cd target/universal/stage
OUTFOLDER_BASE=../../../$OUTFOLDER_BASE

# TYPE_I_RANGE=(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8)
# TYPE_II_RANGE=(0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8)
TYPE_I_RANGE=(0 0.4)
TYPE_II_RANGE=(0 0.4)

for replicate in `seq 1 $REPLICATES`
do
  for TYPE_I_RATIO in ${TYPE_I_RANGE[*]}
  do
    for TYPE_II_RATIO in ${TYPE_II_RANGE[*]}
    do
      ./bin/workflow \
        --outfolder $OUTFOLDER_BASE/noise-and-resolution/Type_I_Errors=$TYPE_I_RATIO-Type_II_Errors=$TYPE_II_RATIO/replicate-$replicate \
        --type-i-error-ratio $TYPE_I_RATIO \
        --type-ii-error-ratio $TYPE_II_RATIO \
        --random-seed $replicate \
        $*
    done
  done
done
