package karme.evaluation

import org.scalactic.TolerantNumerics
import org.scalatest.FunSuite

class HypergeometricTestTest extends FunSuite {
  test("hypergeometric test") {
    val resultPValue = new HypergeometricTest().test(nbSamples = 10,
      nbSampleSuccesses = 5, nbTotalSuccesses = 100, nbTotalFailures = 100)

    val expected = 0.6262351

    val equality = TolerantNumerics.tolerantDoubleEquality(0.0001)
    assert(equality.areEqual(expected, resultPValue))
  }
}
