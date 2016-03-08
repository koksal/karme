# options(warn = 1)

# suppressPackageStartupMessages(library(MSBVAR))
suppressPackageStartupMessages(library(vars))

args = commandArgs(trailingOnly = TRUE)
inputDataFile     = args[[1]]
inputProteinFile  = args[[2]]
lag               = as.integer(args[[3]])
outputFolder      = args[[4]]

# create subdir for cluster output
# dir.create(outputFolder)

proteins = readLines(inputProteinFile)

readData <- function(fn) {
  d = read.csv(fn, check.names = FALSE)
  return(d)
}

testGranger <- function(d, p1, p2) {
  print("Testing:")
  print(p1)
  print(p2)

  # order by pseudotime
  sortedD <- d[with(d, order(Pseudotime)),]
  p1Values = sortedD[,p1]
  p2Values = sortedD[,p2]
  m = as.matrix(p1Values)
  m = cbind(m, p2Values)
  colnames(m) = c(p1, p2)

  vs = VARselect(m, lag.max = 100)
  print(vs)
  
  selectedLag = vs$selection['AIC(n)'][[1]]
  print(paste("Selected lag: ", selectedLag))
  v = VAR(m, p = selectedLag)
  # print(v)
  p1CausesP2 = causality(v, cause = make.names(p1))
  p2CausesP1 = causality(v, cause = make.names(p2))
  
  print(p1CausesP2$Granger)
  print(p2CausesP1$Granger)
  # VARresult = VAR(m, p = lag, type = "none")
  # grangerResult = causality(VARresult, cause = p1)$Granger
  # result = granger.test(m, p = as.integer(lag))
  # print(grangerResult)
  # return(result)
}

testAllPairwise <- function() {
  d = readData(inputDataFile)

  results = c()

  testGranger(d, proteins[1], proteins[2])
  for (i in 1:length(proteins)) {
    for (j in 1:length(proteins)) {
      if (i < j) {
        # res = testGranger(d, proteins[i], proteins[j])
        # results = rbind(results, res)
      }
    }
  }

  # fname = paste(outputFolder, "/", "granger.tsv", sep = "")
  # write.table(
  #             results,
  #             file = fname,
  #             sep = "\t",
  #             row.names = TRUE,
  #             col.names = TRUE
  # )
}

testAllPairwise()
