package karme.evaluation

import karme.util.TimingUtil
import org.scalactic.TolerantNumerics
import org.scalatest.FunSuite

import scala.util.Random

class RankSumTestTest extends FunSuite {

  val equality = TolerantNumerics.tolerantDoubleEquality(0.0001)

  test("rank sum test") {
    val xs = (1 to 100) map (_ => 10.0)
    val ys = (1 to 100) map (_ => 20.0)

    val rankSumTest = new RankSumTest

    val testXsGreater = rankSumTest.test(xs, ys)
    val testYsGreater = rankSumTest.test(ys, xs)

    assert(equality.areEqual(0, testXsGreater.statistic))
    assert(equality.areEqual(1, testXsGreater.pValue))

    assert(equality.areEqual(10000, testYsGreater.statistic))
    assert(testYsGreater.pValue < 0.01)
  }

  test("rank sum test on subsample") {
    val rand = new Random(0)
    val dist = (1 to 100) map (_ => rand.nextDouble())
    val subsample = dist.sorted.reverse.take(20)

    val rankSum = new RankSumTest

    val testSampleGreater = rankSum.test(subsample, dist)
    val testDistGreater = rankSum.test(dist, subsample)

    assert(testSampleGreater.statistic > testDistGreater.statistic)
    assert(testSampleGreater.pValue < testDistGreater.pValue)
  }

  test("rank sum test with single-element samples") {
    val xs = List(1.0)
    val ys = List(2.0)

    val rankSum = new RankSumTest
    
    val testXsGreater = rankSum.test(xs, ys)
    val testYsGreater = rankSum.test(ys, xs)

    assert(equality.areEqual(1, testXsGreater.pValue))
    assert(equality.areEqual(0.5, testYsGreater.pValue))
  }

}
