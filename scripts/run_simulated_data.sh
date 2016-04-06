#!/bin/bash
TIMESTAMP=`date +%F-%H-%M-%S`
OUTFOLDER=./evaluation/${TIMESTAMP}
if [ "$1" != "" ]; then
  LABEL=$1
  OUTFOLDER=${OUTFOLDER}-${LABEL}
fi

for iter in 10;
do
  for speedCoefSD in "0.25" "1";
  do
    for noiseSD in "0.25" "1";
    do
      for tw in "0";
      do
        for nbs in 10;
        do
          label="iter-$iter-speedSD-$speedCoefSD-noiseSD-$noiseSD-timeW-$tw-neighbors-$nbs"
          ./scripts/run.sh \
            --proteins data/simulated/names.txt \
            --simulate \
            --evaluate \
            --speedCoefSD $speedCoefSD \
            --noiseSD $noiseSD \
            --outlabel $label \
            --outfolder ${OUTFOLDER} \
            --seed 0 \
            --sample 10000 \
            --arcsinh 5 \
            --alpha 0.5 \
            --neighbors $nbs \
            --timeweight $tw \
            --iterations $iter
        done
      done
    done
  done
done
