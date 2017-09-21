package karme.simulation

import karme.synthesis.FunctionTrees
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.util.MapUtil

object AsyncBooleanNetworkSimulation {

  val SIMULATION_DEPTH_LIMIT = 100

  def simulateWithTimestamps(
    functions: Map[String, FunExpr],
    initialStates: Set[ConcreteBooleanState]
  ): Set[(ConcreteBooleanState, Set[Int])] = {
    var stateToTimestamps = Map[ConcreteBooleanState, Set[Int]]()

    var currentStates = Set.empty[ConcreteBooleanState]
    var nextStates = initialStates

    var step = 0
    while (currentStates != nextStates && step < SIMULATION_DEPTH_LIMIT) {
      currentStates = nextStates

      for (state <- currentStates) {
        stateToTimestamps = MapUtil.addBinding(stateToTimestamps, state, step)
      }

      nextStates = currentStates flatMap { s =>
        functions map {
          case (label, fun) => {
            updatedState(label, fun, s)
          }
        }
      }

      step += 1
    }

    if (currentStates != nextStates) {
      println("Fixpoint not reached in simulation.")
    } else {
      println(s"Fixpoint reached in $step steps in simulation.")
    }

    stateToTimestamps.toSet
  }

  def simulate(
    functions: Map[String, FunExpr],
    initialStates: Set[ConcreteBooleanState]
  ): Set[ConcreteBooleanState] = {
    simulateWithTimestamps(functions, initialStates).map(_._1)
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
