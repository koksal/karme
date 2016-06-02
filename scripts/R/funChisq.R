options(warn = 1)

library(FunChisq)

args = commandArgs(trailingOnly = TRUE)
inputFile   = args[[1]]
statisticF  = args[[2]]
pValueF     = args[[3]]
estimateF   = args[[4]]

# read contingency table
d = read.csv(inputFile, header = FALSE)

result = fun.chisq.test(d, method = "normalized")

write(result$statistic, file = statisticF)
write(result$p.value, file = pValueF)
write(result$estimate, file = estimateF)
