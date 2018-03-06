#!/bin/bash

if [ $# -eq 0 ]
then
  echo "Parent output folder not given."
  exit 1
else
  parent=$1/partial-targets
fi

for f1 in $parent/*
do
  twoDimFolder=$(basename "$f1")

  for f2 in $parent/$twoDimFolder/*
  do
    replicateFolder=$(basename "$f2")

    for f3 in $parent/$twoDimFolder/$replicateFolder/*
    do
      hiddenVarFolder=$(basename "$f3")

      mkdir -p $parent/$hiddenVarFolder/$twoDimFolder
      cp -r \
        $parent/$twoDimFolder/$replicateFolder/$hiddenVarFolder \
        $parent/$hiddenVarFolder/$twoDimFolder/$replicateFolder
    done
  done
done
