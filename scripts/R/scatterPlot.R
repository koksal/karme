options(warn = 1)
library(ggplot2)

args = commandArgs(trailingOnly = TRUE)
inputFile   = args[[1]]
outputFile  = args[[2]]

data = read.csv(inputFile, check.names = FALSE)

xCol = data[, "x"] # log(data[, "x"], base = exp(1))
yCol = data[, "y"]

if ("group" %in% colnames(data)) {
  groupCol = data[, "group"]

  df = data.frame(x = xCol, y = yCol, group = groupCol)

  p <- ggplot() +
    geom_point(
               data = df,
               aes(x = x, y = y, colour = group)
               ) +
    scale_colour_gradient(low = "green", high = "red")
} else {
  df = data.frame(x = xCol, y = yCol)

  p <- ggplot() +
    geom_point(
               data = df,
               aes(x = x, y = y)
               )
}

ggsave(file = outputFile)
