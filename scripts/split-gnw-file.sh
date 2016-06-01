#!/bin/bash

FILE=$1
DIGITS=5 # for 10k cells
PREFIX="cell"

echo Splitting files...
csplit --prefix=$PREFIX --digits=$DIGITS $FILE '/^$/' '{*}'

echo Removing first file...
rm ${PREFIX}00001

echo Removing empty lines from files...
for f in ${PREFIX}*
do
  sed -i '/^$/d' $f
done
