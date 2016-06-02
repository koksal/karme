options(warn = 1)

library(Ckmeans.1d.dp)

args = commandArgs(trailingOnly = TRUE)
inputFile   = args[[1]]
outputFile  = args[[2]]

xs = sapply(readLines(inputFile), function (l) as.double(l))

clusterLimits = c(3, 9)
result = Ckmeans.1d.dp(xs, clusterLimits)

write.table(
            result$cluster,
            file = outputFile,
            sep = ",",
            row.names = FALSE,
            col.names = FALSE
            )
