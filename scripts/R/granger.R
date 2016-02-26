options(warn = 1)

library(MSBVAR)

args = commandArgs(trailingOnly = TRUE)
inputDataFile     = args[[1]]
inputProteinFile  = args[[2]]
outputFolder      = args[[3]]

# create subdir for cluster output
dir.create(outputFolder)

proteins = readLines(inputProteinFile)

readData <- function(fn) {
  d = read.csv(fn, check.names = FALSE)
  return(d)
}

testGranger <- function(d, p1, p2) {
  print("Testing:")
  print(p1)
  print(p2)
  # order by pseudotime and compute moving average
  sortedD <- d[with(d, order(Pseudotime)),]
  p1Values = sortedD[,p1]
  p2Values = sortedD[,p2]
  m = as.matrix(p1Values)
  m = cbind(m, p2Values)
  colnames(m) = c(p1, p2)
  result = granger.test(m, p = 50)
  return(result)
}

testAllPairwise <- function() {
  d = readData(inputDataFile)

  results = c()

  for (i in 1:length(proteins)) {
    for (j in 1:length(proteins)) {
      if (i < j) {
        res = testGranger(d, proteins[i], proteins[j])
        results = rbind(results, res)
      }
    }
  }

  fname = paste(outputFolder, "/", "granger.tsv", sep = "")
  write.table(
              results,
              file = fname,
              sep = "\t",
              row.names = TRUE,
              col.names = TRUE
  )
}

testAllPairwise()
