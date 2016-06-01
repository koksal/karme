#!/bin/bash

FILE=$1
DIGITS=5 # for 10k cells
PREFIX="cell"

echo Splitting files...
csplit --prefix=$PREFIX --digits=$DIGITS --silent $FILE '/^$/' '{*}'

HEADERFILE=header
FIRSTFILESUFFIX=$(printf '%*s' "$DIGITS" | tr ' ' '0')
mv ${PREFIX}00000 $HEADERFILE

echo Removing empty lines from files and adding header line...
for f in ${PREFIX}*
do
  TMP=${f}.tmp
  cp $HEADERFILE $TMP
  sed '/^$/d' $f >> $TMP
  mv $TMP $f
done

echo Removing header file...
rm $HEADERFILE
