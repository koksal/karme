libraries = c(
              "Ckmeans.1d.dp", 
              "cluster",
              "ggplot2",
              "gplots",
              "mclust",
              "NbClust",
              "PRROC",
              "RColorBrewer",
              "scales"
              )

for (lib in libraries) {
  install.packages(lib, repos="http://cran.rstudio.com/")
}
