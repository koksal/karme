package karme.transformations

import karme.Experiments.Measurement
import karme.analysis.DiscreteStateAnalysis
import karme.synthesis.Transitions.GenericState
import org.scalatest.FunSuite

class DiscreteStateAnalysisTest extends FunSuite {
  test("Unique states") {
    val map1 = Map("A" -> 1, "B" -> 2, "C" -> 3)
    val map2 = Map("A" -> 3, "B" -> 2, "C" -> 1)
    val ms = Set(
      Measurement("M1", GenericState(map1)),
      Measurement("M2", GenericState(map2)),
      Measurement("M3", GenericState(map1))
    )

    assertResult(2)(DiscreteStateAnalysis.nbUniqueStates(ms))
  }

  test("Discrete cell state distances") {
    val map1 = Map("A" -> 0, "B" -> 0, "C" -> 0)
    val map2 = Map("A" -> 1, "B" -> 2, "C" -> 3)
    val m1 = Measurement("A", GenericState(map1))
    val m2 = Measurement("B", GenericState(map2))

    assertResult(6)(DiscreteStateAnalysis.distance(m1, m2))
  }
}
