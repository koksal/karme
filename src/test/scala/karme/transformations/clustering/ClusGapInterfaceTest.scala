package karme.transformations.clustering

import org.scalatest.FunSuite

class ClusGapInterfaceTest extends FunSuite {
  test("clusGap interface") {
    val matrix: Seq[Seq[Double]] = Seq(
      Seq(1, 1, 1),
      Seq(2, 2, 2),
      Seq(100, 100, 100),
      Seq(101, 101, 101)
    )

    val result = new ClusGapInterface().findGapStatistic(matrix, 3)
    assert(result(0) == result(1))
    assert(result(2) == result(3))
    assert(result(0) != result(2))
  }
}
