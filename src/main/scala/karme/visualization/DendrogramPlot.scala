package karme.visualization

import java.io.File

import karme.Experiments.Experiment
import karme.external.AbstractRInterface
import org.ddahl.rscala.RClient

class DendrogramPlot extends AbstractRInterface {

  override def LIBRARIES = Seq("gplots", "RColorBrewer")

  def plot(
    exp: Experiment[Double], kMax: Int
  ): Unit = {
    val valuesPerVariable = exp.valueMatrix.map(_.toArray).toArray

    R.set("valuesPerVariable", valuesPerVariable)
    R.eval("valuesPerCell = t(valuesPerVariable)")

    R.eval("varClustering = hclust(dist(valuesPerVariable))")
    R.eval("cellClustering = hclust(dist(valuesPerCell))")

    R.eval("varDendrogram = as.dendrogram(varClustering)")
    R.eval("cellDendrogram = as.dendrogram(cellClustering)")

    R.eval(s"varClusterAssignments = cutree(varClustering, k = 1:${kMax})")

    visualizeClustering(R, kMax, ???)
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
