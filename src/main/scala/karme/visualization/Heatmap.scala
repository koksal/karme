package karme.visualization

import java.io.File

import karme.external.AbstractRInterface

class Heatmap extends AbstractRInterface {

  override def LIBRARIES = Seq("gplots")

  def plot(
    matrix: Seq[Seq[Double]],
    xName: String,
    yName: String,
    xLabels: Seq[String],
    yLabels: Seq[String],
    file: File
  ): Unit = {
    R.set("matrix", matrix.map(_.toArray).toArray)
    R.set("xLabels", xLabels.toArray)
    R.set("yLabels", yLabels.toArray)

    R.eval("colnames(matrix) <- xLabels")
    R.eval("rownames(matrix) <- yLabels")

    R.eval(
      """colorPalette = colorRampPalette(c("red", "white", "blue"))
        |(50)
        |
      """.stripMargin)

    R.eval(s"""pdf("${file.getPath}", 8, 8)""")
    R.eval(s"""heatmap.2(
             |          as.matrix(matrix),
             |          Rowv = FALSE,
             |          Colv = FALSE,
             |          col = colorPalette,
             |          xlab = "$xName",
             |          ylab = "$yName",
             |          density.info = 'none',
             |          scale = "none",
             |          trace = "none"
             |          )""".stripMargin)
    R.eval("dev.off()")
  }

}
