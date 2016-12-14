package karme.simulation

import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.Transitions.ConcreteBooleanState

object AsyncBooleanNetworkSimulation {

  def simulate(
    functions: Set[FunExpr],
    initialStates: Set[ConcreteBooleanState]
  ): Set[ConcreteBooleanState] = {
    // for every reachable state, compute all new reachable states using any
    // applicable function.
    var reachableStates = Set.empty[ConcreteBooleanState]
    var processSet = initialStates

    while (processSet.nonEmpty) {
      for (stateToProcess <- processSet) {
        reachableStates += stateToProcess
        processSet -= stateToProcess

        // add any new reachable states to process set
        ???
      }
    }

    reachableStates
  }

}
