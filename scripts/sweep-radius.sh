for r in 20
do
  sbt "run --contexp data/continuous-experiment.csv --clusters data/clustering-renamed.csv --names data/markers.txt --trajectory data/curve1-pseudotime.csv,data/curve2-pseudotime.csv,data/curve3-pseudotime.csv --outfolder radius-$r --window-radius $r"
done
