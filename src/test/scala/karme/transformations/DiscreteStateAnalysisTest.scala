package karme.transformations

import karme.Experiments.Measurement
import org.scalatest.FunSuite

class DiscreteStateAnalysisTest extends FunSuite {
  test("Unique states") {
    val vs1 = List(1, 2, 3)
    val vs2 = List(3, 2, 1)
    val ms = Set(
      Measurement("A", vs1),
      Measurement("B", vs2),
      Measurement("C", vs1)
    )

    assertResult(2)(DiscreteStateAnalysis.nbUniqueStates(ms))
  }

  test("Discrete cell state distances") {
    val m1 = Measurement("A", List(0, 0, 0))
    val m2 = Measurement("B", List(1, 2, 3))

    assertResult(6)(DiscreteStateAnalysis.distance(m1, m2))
  }
}
