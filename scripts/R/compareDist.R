options(warn = 1)


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

compareDistForProt <- function(data, prot, steps) {
  # loop over adjacent steps
  comparisonEnd = length(steps) - 1
  pValueVector = c(prot)
  for (i in 1:length(steps)) {
    if (i <= comparisonEnd) {
      print("Comparing:")
      print(i)
      d1 = subset(data, Step == steps[i])
      d2 = subset(data, Step == steps[i+1])
      d1 = d1[,prot]
      d2 = d2[,prot]
      wt = wilcox.test(d1, d2)
      # print(wt)
      pValueVector[i] = wt[["p.value"]]
    }

    # p <- hist(d1, breaks = 100)
    # pdf(file = paste(outputPath, "/", prot, "-", i, ".pdf", sep = ""))
    # plot(p)
    # dev.off()
  }
  print(pValueVector)
}

# read aggregate data frame
d = read.csv(inputPath)

# get steps
steps = d[, "Step"]
uniqSteps = unique(steps)
print(uniqSteps)

rows = list()
rowNames = list()
# for each protein, see if adjacent steps are significantly different
for (o in obsCols) {
  print("Processing:")
  print(o)
  newRow = compareDistForProt(d, o, uniqSteps)
  rows[[length(rows) + 1]] = newRow
  rowNames[length(rowNames) + 1] = o
}
df = data.frame(matrix(unlist(rows), nrow = length(rows), byrow = T))
row.names(df) = obsCols
write.table(df, file = paste(outputPath, "/adjacent-dist-comparison.tsv", sep=""), sep="\t", quote=FALSE, col.names=NA)
