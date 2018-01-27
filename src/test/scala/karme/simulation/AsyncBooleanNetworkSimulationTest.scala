package karme.simulation

import karme.synthesis.FunctionTrees.{FunConst, FunExpr, FunVar}
import karme.synthesis.Transitions.GenericState
import org.scalatest.FunSuite

class AsyncBooleanNetworkSimulationTest extends FunSuite {

  val model1 = Map[String, FunExpr](
    "A" -> FunVar("B"),
    "B" -> FunVar("B")
  )

  val initStates1 = Set(
    GenericState(Map(
      "A" -> false,
      "B" -> true
    ))
  )

  val model2 = Map[String, FunExpr](
    "A" -> FunConst(true),
    "B" -> FunConst(true)
  )

  val initStates2 = Set(
    GenericState(Map(
      "A" -> false,
      "B" -> false
    ))
  )

  test("one-step simulation with two functions") {
    val expected = Set(
      GenericState(Map(
        "A" -> false,
        "B" -> true
      )),
      GenericState(Map(
        "A" -> true,
        "B" -> true
      ))
    )

    assertResult(expected)(
      AsyncBooleanNetworkSimulation.simulateOneStep(model1, initStates1))
  }

  test("one-step simulation with timestamps") {
    val expected = Set(
      (
        GenericState(Map(
          "A" -> false,
          "B" -> true
        )),
        List(0)
      ),
      (
        GenericState(Map(
          "A" -> true,
          "B" -> true
        )),
        List(1)
      )
    )

    assertResult(expected)(AsyncBooleanNetworkSimulation
      .simulateOneStepWithTimestamps(model1, initStates1))
  }

  test("check any-step timestamps for a two-state simulation") {
    val expected = Set(
      (
        GenericState(Map(
          "A" -> false,
          "B" -> true
        )),
        List(0)
      ),
      (
        GenericState(Map(
          "A" -> true,
          "B" -> true
        )),
        List(1)
      )
    )

    assertResult(expected)(AsyncBooleanNetworkSimulation
      .simulateAnyStepsWithTimestamps(model1, initStates1))
  }

  test("check any-step timestamps for a four-state simulation") {
    val expected = Set(
      (
        GenericState(Map(
          "A" -> false,
          "B" -> false
        )),
        List(0)
      ),
      (
        GenericState(Map(
          "A" -> true,
          "B" -> false
        )),
        List(1)
      ),
      (
        GenericState(Map(
          "A" -> false,
          "B" -> true
        )),
        List(1)
      ),
      (
        GenericState(Map(
          "A" -> true,
          "B" -> true
        )),
        List(1, 2)
      )
    )

    assertResult(expected)(AsyncBooleanNetworkSimulation
      .simulateAnyStepsWithTimestamps(model2, initStates2))
  }

}
