package karme.util

import org.scalatest.FunSuite

class MathUtilTest extends FunSuite {
  test("Median") {
    assertResult(1)(MathUtil.median(List(1)))
    assertResult(2)(MathUtil.median(List(3, 1, 2)))
    assertResult(2.5)(MathUtil.median(List(2, 4, 1, 3)))
  }
}
