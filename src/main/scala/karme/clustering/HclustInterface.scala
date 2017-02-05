package karme.clustering

import java.io.File

import karme.Experiments.Experiment
import org.ddahl.rscala.RClient

object HclustInterface {

  def computeClusterCuts(
    exp: Experiment[Double], kMax: Int, outFolder: File
  ): Seq[Map[String, Int]] = {
    assert(kMax >= 1)

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

    R.eval(s"varClusterAssignments = cutree(varClustering, k = 1:${kMax})")

    // a matrix where each column corresponds to one element of vector k
    val varClusterAssignments = R.getI2("varClusterAssignments")
    assert(varClusterAssignments.size == kMax)
    assert(exp.names.size == varClusterAssignments.size)

    visualizeClustering(R, kMax, outFolder)
    R.exit()

    varClusterAssignments.transpose map { assignmentForCluster =>
      exp.names.zip(assignmentForCluster).toMap
    }
  }

  private def visualizeClustering(
    R: RClient,
    kMax: Int,
    outFolder: File
  ): Unit = {
    R.eval("continuousPalette = " +
      "colorRampPalette(c(\"blue\", \"white\", \"red\"))(100)")
    val nbColorRepetition = kMax / 12 + 1
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
  }


}
