#!/bin/bash

if [ $# -eq 0 ]
then
  echo "Parent output folder not given."
  exit 1
else
  PARENT_OUTPUT_FOLDER=$1
fi

sbt stage
cd target/universal/stage
PARENT_OUTPUT_FOLDER=../../../$PARENT_OUTPUT_FOLDER

FILES=(
  "stable-states-wildtype.tsv" 
  "stable-states-knockouts.tsv"
  "reachable-states-wildtype.tsv" 
  "reachable-states-knockouts.tsv"
  "sampled-states.tsv"
  "reconstructed-states.tsv"
  "function-behavior-similarity.tsv"
  "function-io-pair-similarity.tsv"
)

# for EVAL_TYPE in "noise" "resolution" "noise-and-resolution" "partial-targets"
for EVAL_TYPE in "partial-targets"
do
  for FILE in ${FILES[*]}
  do
    echo "Aggregating" $FILE
    echo "Eval type: " $EVAL_TYPE

    if [ -d "$PARENT_OUTPUT_FOLDER/$EVAL_TYPE" ]; then

      # echo "Running table aggregation"
      # if [ $EVAL_TYPE = "partial-targets" ]
      # then
      #   for FOLDER in $PARENT_OUTPUT_FOLDER/$EVAL_TYPE/*/*=*
      #   do
      #     ./bin/table-aggregation \
      #       $FOLDER/$FILE \
      #       $FOLDER/replicate-*/$FILE
      #   done
      # else
      #   for FOLDER in $PARENT_OUTPUT_FOLDER/$EVAL_TYPE/*=*
      #   do
      #     ./bin/table-aggregation \
      #       $FOLDER/$FILE \
      #       $FOLDER/replicate-*/$FILE
      #   done
      # fi

      echo "Running heatmap aggregation"
      if [ $EVAL_TYPE = "partial-targets" ]
      then
        # for FOLDER in $PARENT_OUTPUT_FOLDER/$EVAL_TYPE/*
        # do
        #   if [ -d "$FOLDER" ]
        #   then
        #     ./bin/heatmap-aggregation \
        #       $FOLDER/$FILE \
        #       $FOLDER/*/$FILE
        #   fi
        # done
        ./bin/box-plot-aggregation \
          $PARENT_OUTPUT_FOLDER/$EVAL_TYPE/$FILE \
          $PARENT_OUTPUT_FOLDER/$EVAL_TYPE/*=*/*/$FILE
      elif [ $EVAL_TYPE = "noise-and-resolution" ]
      then
        ./bin/heatmap-aggregation \
          $PARENT_OUTPUT_FOLDER/$EVAL_TYPE/$FILE \
          $PARENT_OUTPUT_FOLDER/$EVAL_TYPE/*/$FILE
      else
        ./bin/box-plot-aggregation \
          $PARENT_OUTPUT_FOLDER/$EVAL_TYPE/$FILE \
          $PARENT_OUTPUT_FOLDER/$EVAL_TYPE/*/$FILE
      fi
    fi
  done
done
