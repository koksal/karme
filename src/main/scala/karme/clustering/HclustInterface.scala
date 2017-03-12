package karme.clustering

import java.io.File

import karme.Experiments.Experiment
import karme.external.AbstractRInterface
import org.ddahl.rscala.RClient

class HclustInterface(
  exp: Experiment[Double], kMax: Int, outFolder: File
) extends AbstractRInterface[Seq[Map[String, Int]]] {

  override val LIBRARIES = Seq("gplots", "RColorBrewer")

  def process(R: RClient): Seq[Map[String, Int]] = {
    assert(kMax >= 1)

    // transform experiment to 2D array
    val valuesPerVariable = exp.valueMatrix.map(_.toArray).toArray

    R.set("valuesPerVariable", valuesPerVariable)
    R.eval("valuesPerCell = t(valuesPerVariable)")

    R.eval("varClustering = hclust(dist(valuesPerVariable))")
    R.eval("cellClustering = hclust(dist(valuesPerCell))")

    R.eval("varDendrogram = as.dendrogram(varClustering)")
    R.eval("cellDendrogram = as.dendrogram(cellClustering)")

    R.eval(s"varClusterAssignments = cutree(varClustering, k = 1:${kMax})")

    // a matrix where each column corresponds to one element of vector k
    val varClusterAssignments = R.getI2("varClusterAssignments")
    val assignmentsPerK = varClusterAssignments.transpose
    assert(assignmentsPerK.size == kMax)

    // TODO visualize for all k
    // visualizeClustering(R, kMax, outFolder)

    assignmentsPerK map { assignment =>
      assert(assignment.size == exp.names.size)
      exp.names.zip(assignment).toMap
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
