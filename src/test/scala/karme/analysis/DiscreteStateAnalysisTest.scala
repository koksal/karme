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
}
