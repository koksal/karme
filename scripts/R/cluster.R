options(warn = 1)
library(tightClust)

args = commandArgs(trailingOnly = TRUE)
parent = args[[1]]
inputFile = args[[2]]
outputFolder = args[[3]]

inputPath = paste(parent, "/", inputFile, sep = "")
outputPath = paste(parent, "/", outputFolder, sep = "")

# create subdir for cluster output
dir.create(outputPath)

obsCols = c(
  "pCD3z.Lu175.Dd",
  "pSLP76.Gd156.Dd",
  "pErk1_2.Er167.Dd",
  "pS6.Yb172.Dd",
  "pCreb.Yb176.Dd",
  "pMAPKAPKII.Eu153.Dd",
  "Ikba.Er166.Dd",
  "pNFKb.Ho165.Dd",
  "pRb.Gd158.Dd",
  "pFAK.Nd148.Dd",
  "pAkt_S473.Tb159.Dd"
)

processFileStep <- function(fn, step) {
  # filter to relevant step
  d = d[d$Step %in% c(step), ]
  # filter to protein columns
  obsFrame = d[, obsCols]

  # create a matrix
  obsMatrix = data.matrix(obsFrame)
  # clustering
  nbTargets = 20
  kMin      = nbTargets + 5
  clusters <- tightClust::tight.clust(obsMatrix, target=nbTargets, k.min=kMin)

  # tightClust::plot.tight.clust(clusters)

  # write output
  clusterFn <- paste(outputPath, "/targetClusters-", nbTargets, "-step-", step, ".tsv", sep="")
  tightClust::write.tight.clust(clusters, clusterFn, sep = "\t", quote = FALSE, row.names = FALSE)
}

# read aggregate data frame
d = read.csv(inputPath)

# get steps
steps = d[, "Step"]
uniqSteps = unique(steps)
print(uniqSteps)
for (s in uniqSteps) {
  print("Processing step: ")
  print(s)
  processFileStep(d, s)
}
