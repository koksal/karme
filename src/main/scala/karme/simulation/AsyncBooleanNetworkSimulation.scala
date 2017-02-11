package karme.simulation

import karme.synthesis.FunctionTrees
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.Transitions.ConcreteBooleanState

object AsyncBooleanNetworkSimulation {

  val SIMULATION_DEPTH_LIMIT = 100

  def pickFunctionsAndSimulate(
    labelToFunctions: Map[String, Set[FunExpr]],
    initialStates: Set[ConcreteBooleanState]
  ): Set[ConcreteBooleanState] = {
    val chosenFunctions = labelToFunctions collect {
      case (label, fs) if fs.nonEmpty => (label, fs.head)
    }
    simulate(chosenFunctions, initialStates)
  }

  private def simulate(
    functions: Map[String, FunExpr],
    initialStates: Set[ConcreteBooleanState]
  ): Set[ConcreteBooleanState] = {
    // for every reachable state, compute all new reachable states using any
    // applicable function.
    var reachableStates = Set.empty[ConcreteBooleanState]
    var processSet = initialStates

    var i = 0
    while (processSet.nonEmpty && i < SIMULATION_DEPTH_LIMIT) {
      i += 1
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

        val unseenStates = statesViaFunctionApplication.toSet --
          (reachableStates ++ processSet)

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
    new ConcreteBooleanState(newMapping)
  }

}
