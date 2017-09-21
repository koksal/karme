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

  test("simulate two functions") {
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
      AsyncBooleanNetworkSimulation.simulate(labelToFun, initStates))
  }

  test("simulate with timestamps") {
    val expected = Set(
      (
        GenericState(Map(
          "A" -> false,
          "B" -> true
        )),
        Set(0)
      ),
      (
        GenericState(Map(
          "A" -> true,
          "B" -> true
        )),
        Set(1)
      )
    )

    assertResult(expected)(
      AsyncBooleanNetworkSimulation.simulateWithTimestamps(labelToFun,
        initStates))

  }

}
