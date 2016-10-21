package karme

import karme.Experiments.{Experiment, Measurement}
import org.scalatest.FunSuite

class ExperimentsTest extends FunSuite {

  test("Experiment projection") {
    val e = Experiment(
      List("A", "B", "C"),
      List(Measurement("M1", List(1, 2, 3)))
    )

    val expected = Experiment(
      List("C", "A"),
      List(Measurement("M1", List(3, 1)))
    )

    assertResult(expected)(e.project(List("C", "A")))
  }

}
