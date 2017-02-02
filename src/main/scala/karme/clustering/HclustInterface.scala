package karme.clustering

import java.io.File

import karme.Experiments.Experiment
import org.ddahl.rscala.RClient

object HclustInterface {

  def clusterAndCutree(
    exp: Experiment[Double], k: Int, outFolder: File
  ): Map[String, Int] = {
    val R = RClient()
    R eval "library(gplots)"
    R eval "library(RColorBrewer)"

    // transform experiment to 2D array
    val valuesPerVariable = exp.names.toArray map { n =>
      exp.valuesForName(n).toArray
    }

    R.set("valuesPerVariable", valuesPerVariable)
    R.eval("valuesPerCell = t(valuesPerVariable)")

    R.eval("varClustering = hclust(dist(valuesPerVariable))")
    R.eval("cellClustering = hclust(dist(valuesPerCell))")

    R.eval("varDendrogram = as.dendrogram(varClustering)")
    R.eval("cellDendrogram = as.dendrogram(cellClustering)")

    R.eval(s"varClusterAssignments = cutree(varClustering, k = ${k})")

    R.eval("continuousPalette = " +
      "colorRampPalette(c(\"blue\", \"white\", \"red\"))(100)")
    val nbColorRepetition = k / 12 + 1
    R.eval(
      s"""discretePalette =
         |  rep(brewer.pal(12, "Set3"), ${nbColorRepetition})""".stripMargin)

    val visFilename = new File(outFolder, "clustering-heatmap.pdf").getPath()
    R.eval(s"""pdf("${visFilename}", 10, 10)""")
    R.eval("""heatmap.2(
             |          as.matrix(valuesPerCell),
             |          scale = "none",
             |          Colv = varDendrogram,
             |          Rowv = cellDendrogram,
             |          col = continuousPalette,
             |          trace = "none",
             |          ColSideColors = discretePalette[varClusterAssignments]
             |          )""".stripMargin)
    R.eval("dev.off()")

    val varClusterAssignments = R.getI1("varClusterAssignments")
    assert(exp.names.size == varClusterAssignments.size)

    exp.names.zip(varClusterAssignments).toMap
  }

}
