#!/bin/bash
TIMESTAMP=`date +%F-%H-%M-%S`
OUTFOLDER=./evaluation/${TIMESTAMP}
if [ "$1" != "" ]; then
  LABEL=$1
  OUTFOLDER=${OUTFOLDER}-${LABEL}
fi

for iter in 10 100;
do
  for speedCoefSD in 1 2;
  do
    for noiseSD in 0 1;
    do
      for tw in 0 25 50 75;
      do
        for nbs in 5 10 100;
        do
          label="iter-$iter-speedSD-$speedCoefSD-noiseSD-$noiseSD-timeW-$tw-neighbors-$nbs"
          ./scripts/run.sh \
            --proteins data/test/names.txt \
            --simulate \
            --evaluate \
            --speedCoefSD $speedCoefSD \
            --noiseSD $noiseSD \
            --outlabel $label \
            --outfolder ${OUTFOLDER} \
            --seed 0 \
            --arcsinh 5 \
            --alpha 0.5 \
            --neighbors $nbs \
            --timeweight 0.$tw \
            --iterations $iter
        done
      done
    done
  done
done
