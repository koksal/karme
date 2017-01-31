options(warn = 1)
library(gplots)
library(RColorBrewer)

args = commandArgs(trailingOnly = TRUE)
inputFile = args[[1]]

d = read.csv(inputFile)

cellIDs = d[, "id"]
cellValues = d[, -1]
geneValues = t(cellValues)
cellClustering = hclust(dist(cellValues))
geneClustering = hclust(dist(geneValues))
cellDendro = as.dendrogram(cellClustering)
geneDendro = as.dendrogram(geneClustering)

k = 3

continuousPalette = colorRampPalette(c("blue", "white", "red"))(100)
discretePalette = brewer.pal(k, "Set3")

pdf("heatmap.pdf", 10, 10)
heatmap.2(
          as.matrix(cellValues), 
          scale = "none", 
          Colv = geneDendro, 
          Rowv = cellDendro, 
          col = continuousPalette,
          trace = "none", 
          ColSideColors = discretePalette[cutree(geneClustering, k = k)]
          )
dev.off()
