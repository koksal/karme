package karme.transformations

import org.scalactic.TolerantNumerics
import org.scalatest.FunSuite

class KolmogorovSmirnovTestTest extends FunSuite {

  val equality = TolerantNumerics.tolerantDoubleEquality(0.0001)

  test("KS test") {
    val xs = (1 to 100) map (_ => 10.0)
    val ys = (1 to 100) map (_ => 20.0)

    val ksTest = new KolmogorovSmirnovTest

    val testXsGreater = ksTest.testPValue(xs, ys)
    val testYsGreater = ksTest.testPValue(ys, xs)

    assert(equality.areEqual(1, testXsGreater))
    assert(testYsGreater < 0.01)
  }
}
