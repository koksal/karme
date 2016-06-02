options(warn = 1)

library(FunChisq)

args = commandArgs(trailingOnly = TRUE)
inputFile   = args[[1]]
outputFile  = args[[2]]

# read contingency table
d = read.csv(inputFile, header = FALSE)

result = fun.chisq.test(d, method = "normalized")

write(result$statistic, file = outputFile)
