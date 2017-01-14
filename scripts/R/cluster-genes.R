options(warn = 1)
library(NbClust)
# library(proxy)
# library(ggdendro)

args = commandArgs(trailingOnly = TRUE)
inputFile = args[[1]]

d = read.csv(inputFile)

cellIDs = d[, "id"]
cellValues = d[, -1]
valuesPerGene = t(cellValues)
valuesPerGene = head(valuesPerGene, 100)
valuesPerGene = apply(valuesPerGene, 1:2, function(x) asinh(x / 5))

# distMatrix = dist(valuesPerGene, method = 'Pearson')
# clusters <- hclust(distMatrix)
# ggdendrogram(clusters, rotate = FALSE, size = 2)

# geneIDs = rownames(valuesPerGene)
# print(geneIDs)

res<-NbClust(valuesPerGene, diss=NULL, distance = "euclidean", min.nc=2, max.nc=10,
             method = "ward.D", index = "silhouette")

print(res)
