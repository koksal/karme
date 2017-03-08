package karme.visualization

import java.io.File

import org.scalatest.FunSuite

class HistogramTest extends FunSuite {

  test("test histogram plotting to file") {
    val vs = List(1, 2, 2, 3, 3, 3, 4, 4, 5)
    val fillVs = List(false, false, true, true, false, true, true, false, true)
    val f = File.createTempFile("histogram-test", ".pdf", new File("."))

    Histogram.plot(vs, fillVs, f)

    // Only check that the file exists since it differs for each platform.
    assert(f.exists())
  }
}
