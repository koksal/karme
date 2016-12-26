package karme

import karme.Experiments.{Experiment, Measurement}
import karme.synthesis.Transitions.GenericState
import org.scalatest.FunSuite

class ExperimentsTest extends FunSuite {

  test("Experiment projection") {
    val e = Experiment(
      List(Measurement("M1", GenericState(Map("A" -> 1, "B" -> 2, "C" -> 3))))
    )

    val expected = Experiment(
      List(Measurement("M1", GenericState(Map("A" -> 1, "C" -> 3))))
    )

    assertResult(expected)(e.project(Set("C", "A")))
  }

}
