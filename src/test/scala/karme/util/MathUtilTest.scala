package karme.util

import org.scalactic.TolerantNumerics
import org.scalatest.FunSuite

class MathUtilTest extends FunSuite {
  test("Median") {
    assertResult(1)(MathUtil.median(List(1)))
    assertResult(2)(MathUtil.median(List(3, 1, 2)))
    assertResult(2.5)(MathUtil.median(List(2, 4, 1, 3)))
  }

  test("Standard deviation") {
    val values = List[Double](10,2,38,23,38,23,21)

    val equality = TolerantNumerics.tolerantDoubleEquality(0.0001)
    assert(equality.areEqual(13.28443, MathUtil.stdev(values)))
  }

}
