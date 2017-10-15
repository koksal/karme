package karme.simulation

import karme.synthesis.FunctionTrees.{FunExpr, FunVar}
import karme.synthesis.Transitions.GenericState
import org.scalatest.FunSuite

class AsyncBooleanNetworkSimulationTest extends FunSuite {

  val labelToFun = Map[String, FunExpr](
    "A" -> FunVar("B"),
    "B" -> FunVar("B")
  )

  val initStates = Set(
    GenericState(Map(
      "A" -> false,
      "B" -> true
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
      AsyncBooleanNetworkSimulation.simulateOneStep(labelToFun, initStates))
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
      .simulateOneStepWithTimestamps(labelToFun, initStates))
  }

  test("any-step simulation with timestamps") {
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
      .simulateAnyStepsWithTimestamps(labelToFun, initStates))
  }

}
