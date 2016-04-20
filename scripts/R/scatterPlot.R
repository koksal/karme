options(warn = 1)
library(ggplot2)

args = commandArgs(trailingOnly = TRUE)
inputFile   = args[[1]]
outputFile  = args[[2]]

data = read.csv(inputFile, check.names = FALSE)

xCol = data[, "x"] # log(data[, "x"], base = exp(1))
yCol = data[, "y"]
df = data.frame(x = xCol, y = yCol)

p <- ggplot() +
  geom_point(
             data = df,
             aes(x = x, y = y)
             )

ggsave(file = outputFile)
