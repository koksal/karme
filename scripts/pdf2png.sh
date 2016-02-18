#!/bin/zsh

for f in $1/**/*.pdf
do
  convert -density 150 $f -quality 90 ${f}.png && rm $f
done
