package karme.visualization

import java.io.File

import org.scalatest.FunSuite

class HeatmapTest extends FunSuite {

  test("heatmap plotting without manual breaks") {
    val matrix = List(
      List(1.0, 2.0, 3.0),
      List(4.0, 5.0, 6.0),
      List(7.0, 8.0, 9.0)
    )

    val f = File.createTempFile("heatmap-test", ".pdf")
    f.deleteOnExit()

    val xLabels = List("x1", "x2", "x3")
    val yLabels = List("y1", "y2", "y3")

    new Heatmap().plot(matrix, "x", "y", xLabels, yLabels, f)

    assert(f.exists())
  }

  test("heatmap plotting with manual breaks") {
    val matrix = List(
      List(1.0, 2.0, 3.0),
      List(4.0, 5.0, 6.0),
      List(7.0, 8.0, 9.0)
    )

    val f = File.createTempFile("heatmap-test", ".pdf")
    f.deleteOnExit()

    val xLabels = List("x1", "x2", "x3")
    val yLabels = List("y1", "y2", "y3")

    val limits = Some((0.0, 10.0))

    new Heatmap().plot(matrix, "x", "y", xLabels, yLabels, limits, f)

    assert(f.exists())
  }
}
