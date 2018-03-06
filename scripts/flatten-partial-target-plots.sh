#!/bin/bash

if [ $# -eq 0 ]
then
  echo "Parent output folder not given."
  exit 1
else
  parent=$1
fi

for f in $parent/partial-targets/*
do
  innerName=$(basename "$f")
  for pdfFile in $f/*.pdf
  do
    baseName=$(basename "$pdfFile")
    extension="${baseName##*.}"
    nameWithoutExt="${baseName%.*}"
    mkdir -p "$parent/partial-targets/$nameWithoutExt"
    cp "$pdfFile" "$parent/partial-targets/$nameWithoutExt/$innerName.$extension"
  done
done

