package karme.analysis

import karme.DiscreteCellMeasurement
import org.scalatest.FunSuite

class DiscreteStateAnalysisTest extends FunSuite {
  test("Unique states") {
    val vs1 = List(1, 2, 3)
    val vs2 = List(3, 2, 1)
    val ms = Set(
      DiscreteCellMeasurement("A", vs1),
      DiscreteCellMeasurement("B", vs2),
      DiscreteCellMeasurement("C", vs1)
    )

    assertResult(2)(DiscreteStateAnalysis.nbUniqueStates(ms))
  }

  test("Discrete cell state distances") {
    val m1 = DiscreteCellMeasurement("A", List(0, 0, 0))
    val m2 = DiscreteCellMeasurement("B", List(1, 2, 3))

    assertResult(6)(DiscreteStateAnalysis.distance(m1, m2))
  }
}
