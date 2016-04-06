options(warn = 1)
library(ggplot2)

args = commandArgs(trailingOnly = TRUE)
inputFile = args[[1]]
outputFolder = args[[2]]
name = args[[3]]

# create subdir for cluster output
dir.create(outputFolder)

data = read.csv(inputFile, check.names = FALSE)

xCol = log(data[, "x"], base = exp(1))
yCol = data[, "y"]
df = data.frame(x = xCol, y = yCol)

p <- ggplot() +
  geom_point(
             data = df,
             aes(x = x, y = y)
             )

fname = paste(outputFolder, "/", name, "-neighbors.pdf", sep = "")
ggsave(file = fname)
