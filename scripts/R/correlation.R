options(warn = 1)

args = commandArgs(trailingOnly = TRUE)
inputFile   = args[[1]]
xName       = args[[2]]
yName       = args[[3]]
outputFile  = args[[4]]

# read file
d = read.csv(inputFile, check.names = FALSE)

# extract x and y columns
x = d[, xName]
y = d[, yName]

# call corr
result = cor.test(x, y, method = "spearman")$estimate

# write value into file
write(result, file = outputFile)
