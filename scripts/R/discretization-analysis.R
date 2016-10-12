library(ggplot2)
library(Ckmeans.1d.dp)
library(diptest)
suppressPackageStartupMessages(library(mclust))

data = read.csv("~/Desktop/OEcounts.csv")
# drop first column, the cell name
names = tail(names(data), -1)

cat(paste("gene", "ckmeans # clusters", "dip test statistic", "dip test p-value", "mclust # clusters", sep = ","))
cat("\n")

for (n in names) {
  vs = asinh(data[[n]]/2) / log(10)

  ckmeansResult = suppressWarnings(Ckmeans.1d.dp(vs, k = c(1, 2)))
  min = min(ckmeansResult$cluster)
  max = max(ckmeansResult$cluster)
  nbLevels = max - min + 1

  dipTestResult = dip.test(vs)

  mclustResult = Mclust(vs, G=1:2)

  cat(paste(n, nbLevels, dipTestResult$statistic, dipTestResult$p.value, mclustResult$G, sep = ","))
  cat("\n")

  plot = qplot(vs, data = NULL, geom = "histogram")

  if (nbLevels == 1) {
    plotFname = paste("histograms/unimodal/", n, ".pdf", sep = "")
  } else {
    plotFname = paste("histograms/bimodal/", n, ".pdf", sep = "")
  }

  ggsave(plot, file = plotFname)
}
