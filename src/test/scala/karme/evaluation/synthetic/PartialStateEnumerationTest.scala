package karme.evaluation.synthetic

import karme.synthesis.Transitions.GenericState
import org.scalatest.FunSuite

class PartialStateEnumerationTest extends FunSuite {

  test("Make partial states by hiding one variable") {
    val completeStates = Set(
      GenericState(Map("a" -> true, "b" -> false)),
      GenericState(Map("a" -> true, "b" -> true))
    )

    val expected = Set(
      (
        Set("b"),
        Set(
          GenericState(Map("a" -> true))
        )
      ),
      (
        Set("a"),
        Set(
          GenericState(Map("b" -> true)),
          GenericState(Map("b" -> false))
        )
      )
    )

    assertResult(expected)(
      PartialStateEnumeration.makePartialStates(completeStates, 1))
  }

}
