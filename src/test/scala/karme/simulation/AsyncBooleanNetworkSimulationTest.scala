package karme.simulation

import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.FunctionTrees.FunVar
import karme.synthesis.Transitions.GenericState
import org.scalatest.FunSuite

class AsyncBooleanNetworkSimulationTest extends FunSuite {

  test("simulate two functions") {
    val labelToFunctions = Map[String, Set[FunExpr]](
      "A" -> Set(FunVar("B")),
      "B" -> Set(FunVar("B"))
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
      AsyncBooleanNetworkSimulation.pickFunctionsAndSimulate(
        labelToFunctions, initStates))
  }

}
