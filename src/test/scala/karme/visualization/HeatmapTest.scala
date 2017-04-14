package karme.visualization

import java.io.File

import org.scalatest.FunSuite

class HeatmapTest extends FunSuite {

  test("heatmap plotting") {
    val matrix = List(
      List(1.0, 2.0, 3.0),
      List(4.0, 5.0, 6.0),
      List(7.0, 8.0, 9.0)
    )

    val f = File.createTempFile("heatmap-test", ".pdf", new File("."))

    val xLabels = List("x1", "x2", "x3")
    val yLabels = List("y1", "y2", "y3")

    new Heatmap(matrix, "x", "y", xLabels, yLabels, f).run()

    assert(f.exists())
  }

}
