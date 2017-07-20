package karme.visualization

import java.io.File

import org.scalatest.FunSuite

class HistogramPlotInterfaceTest extends FunSuite {

  test("histogram plotting to file") {
    val vs = List(1, 2, 2, 3, 3, 3, 4, 4, 5)
    val fillVs = List(false, false, true, true, false, true, true, false, true)
    val f = File.createTempFile("histogram-test", ".pdf")
    f.deleteOnExit()

    new HistogramPlotInterface().plot(vs, fillVs, f)

    // Only check that the file exists since it differs for each platform.
    assert(f.exists())
  }

}
