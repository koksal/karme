package karme.simulation

import karme.synthesis.FunctionTrees
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.Transitions.ConcreteBooleanState

object AsyncBooleanNetworkSimulation {

  def pickFunctionAndSimulate(
    labelToFunctions: Map[String, Set[FunExpr]],
    initialStates: Set[ConcreteBooleanState]
  ): Set[ConcreteBooleanState] = {
    val chosenFunctions = labelToFunctions map {
      case (label, fs) => (label, fs.head)
    }
    simulate(chosenFunctions, initialStates)
  }

  def simulate(
    functions: Map[String, FunExpr],
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
        // apply each function and see if it generates a new state
        val statesViaFunctionApplication = functions map {
          case (label, fun) => {
            updatedState(label, fun, stateToProcess)
          }
        }

        val unseenStates = reachableStates ++ processSet --
          statesViaFunctionApplication

        processSet ++= unseenStates
      }
    }

    reachableStates
  }

  private def updatedState(
    label: String,
    fun: FunExpr,
    inputState: ConcreteBooleanState
  ): ConcreteBooleanState = {
    val funOutput = FunctionTrees.eval(fun, inputState)
    val newMapping = inputState.mapping.updated(label, funOutput)
    ConcreteBooleanState(newMapping)
  }

}
