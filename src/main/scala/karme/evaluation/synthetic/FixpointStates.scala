package karme.evaluation.synthetic

import karme.evaluation.synthetic.stategen.ExhaustiveStateEnumeration
import karme.simulation.AsyncBooleanNetworkSimulation
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.Transitions.ConcreteBooleanState

object FixpointStates {

  def findSimulationFixpoints(
    labelToFun: Map[String, FunExpr],
    initialStates: Set[ConcreteBooleanState]
  ): Set[ConcreteBooleanState] = {
    val simulatedStates = AsyncBooleanNetworkSimulation.simulateOneStep(
      labelToFun, initialStates)

    simulatedStates filter { s =>
      AsyncBooleanNetworkSimulation.stateIsFixpoint(labelToFun, s)
    }
  }

  def findAllFixpoints(
    labelToFun: Map[String, FunExpr]
  ): Set[ConcreteBooleanState] = {
    val allStateSingletons = new ExhaustiveStateEnumeration(
      labelToFun.keySet.toList).enumerateInitialStates()

    val fixpointSingletons = allStateSingletons filter { s =>
      assert(s.size == 1)
      AsyncBooleanNetworkSimulation.stateIsFixpoint(labelToFun, s.head)
    }

    fixpointSingletons.map(_.head).toSet
  }

}
