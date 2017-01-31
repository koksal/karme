options(warn = 1)
library(gplots)
library(RColorBrewer)

args = commandArgs(trailingOnly = TRUE)
inputFile = args[[1]]
markersFile = args[[2]]

d = read.csv(inputFile)
markers = readLines(markersFile)

k = 50

cellIDs = d[, "id"]
cellValues = d[, -1]
geneValues = t(cellValues)
cellClustering = hclust(dist(cellValues))
geneClustering = hclust(dist(geneValues))
cellDendro = as.dendrogram(cellClustering)
geneDendro = as.dendrogram(geneClustering)

geneClusterAssignments = cutree(geneClustering, k = k)
print(geneClusterAssignments[markers])

continuousPalette = colorRampPalette(c("blue", "white", "red"))(100)
discretePalette = rep(brewer.pal(k, "Set3"), 10)

pdf("heatmap.pdf", 10, 10)
heatmap.2(
          as.matrix(cellValues), 
          scale = "none", 
          Colv = geneDendro, 
          Rowv = cellDendro, 
          col = continuousPalette,
          trace = "none", 
          ColSideColors = discretePalette[geneClusterAssignments]
          )
dev.off()
