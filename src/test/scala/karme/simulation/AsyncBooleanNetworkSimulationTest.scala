package karme.simulation

import karme.synthesis.FunctionTrees.{FunExpr, FunVar}
import karme.synthesis.Transitions.GenericState
import org.scalatest.FunSuite

class AsyncBooleanNetworkSimulationTest extends FunSuite {

  test("simulate two functions") {
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

}
