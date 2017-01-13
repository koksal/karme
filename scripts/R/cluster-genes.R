options(warn = 1)
library(NbClust)

args = commandArgs(trailingOnly = TRUE)
inputFile = args[[1]]

d = read.csv(inputFile)

cellIDs = d[, "id"]
cellValues = d[, -1]
valuesPerGene = t(cellValues)
geneIDs = rownames(valuesPerGene)

res<-NbClust(valuesPerGene, diss=NULL, distance = "euclidean", min.nc=2, max.nc=50,
             method = "kmeans", index = "all")

print(res)
