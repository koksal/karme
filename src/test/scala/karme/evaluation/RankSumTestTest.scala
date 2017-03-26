package karme.evaluation

import org.scalatest.FunSuite

class RankSumTestTest extends FunSuite {

  test("rank sum test") {
    val xs = ???
    val ys = ???

    val result = new RankSumTest(xs, ys).run()

    val expectedResult = RankSumTestResult(???, ???)

    assertResult(expectedResult)(result)
  }

}
