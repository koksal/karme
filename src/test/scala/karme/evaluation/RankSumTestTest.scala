package karme.evaluation

import org.scalactic.TolerantNumerics
import org.scalatest.FunSuite

class RankSumTestTest extends FunSuite {

  test("rank sum test") {
    val xs = (1 to 100) map (_ => 10.0)
    val ys = (1 to 100) map (_ => 20.0)

    val testXsGreater = new RankSumTest(xs, ys).run()
    val testYsGreater = new RankSumTest(ys, xs).run()

    val equality = TolerantNumerics.tolerantDoubleEquality(0.0001)

    assert(equality.areEqual(0, testXsGreater.statistic))
    assert(equality.areEqual(1, testXsGreater.pValue))

    assert(equality.areEqual(10000, testYsGreater.statistic))
    assert(testYsGreater.pValue < 0.01)
  }

}
