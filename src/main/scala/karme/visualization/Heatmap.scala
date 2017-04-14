package karme.visualization

import java.io.File

import karme.external.AbstractRInterface
import org.ddahl.rscala.RClient

class Heatmap(
  matrix: Seq[Seq[Double]],
  xName: String,
  yName: String,
  xLabels: Seq[String],
  yLabels: Seq[String],
  file: File
) extends AbstractRInterface[Unit] {

  override val LIBRARIES = Seq("gplots")

  def process(R: RClient): Unit = {
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
