package karme.simulation

import karme.Experiments.Measurement
import karme.graphs.Graphs.UnlabeledDiGraph
import karme.graphs.StateGraphs.{DirectedBooleanStateGraph, StateGraphVertex}
import karme.synthesis.FunctionTrees
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.util.CollectionUtil
import karme.util.{MapUtil, UniqueCounter}

object AsyncBooleanNetworkSimulation {

  val SIMULATION_DEPTH_LIMIT = 100

  def simulateOneStep(
    functions: Map[String, FunExpr],
    initialStates: Set[ConcreteBooleanState]
  ): Set[ConcreteBooleanState] = {
    simulateOneStepWithTimestamps(functions, initialStates).map(_._1)
  }

  def applyFunctionAlternatives(functions: Map[String, FunExpr])(
    s: ConcreteBooleanState
  ): Set[ConcreteBooleanState] = {
    functions.flatMap{
      case (label, fun) => {
        updatedStateIfChanged(label, fun, s)
      }
    }.toSet
  }

  def applyAllFunctionSubsets(functions: Map[String, FunExpr])(
    s: ConcreteBooleanState
  ): Set[ConcreteBooleanState] = {
    val functionsThatChangeInputState = functions filter {
      case (label, fun) => functionChangesState(label, fun, s)
    }

    CollectionUtil.nonEmptySubsets(functionsThatChangeInputState.toSet) map {
      functionSubset => {
        var newState = s
        functionSubset.foreach {
          case (label, fun) => {
            val updatedForFun = updatedState(label, fun, s)
            newState = newState.replaceValue(label, updatedForFun.value(label))
          }
        }
        newState
      }
    }
  }

//  def applyAllFunctions(functions: Map[String, FunExpr])(
//    s: ConcreteBooleanState
//  ): Set[ConcreteBooleanState] = {
//
//  }

  def simulateOneStepWithTimestamps(
    functions: Map[String, FunExpr],
    initialStates: Set[ConcreteBooleanState]
  ): Set[(ConcreteBooleanState, Seq[Int])] = {
    simulateWithTimestamps(initialStates, applyFunctionAlternatives(functions))
  }

  def simulateAnyStepsWithTimestamps(
    functions: Map[String, FunExpr],
    initialStates: Set[ConcreteBooleanState]
  ): Set[(ConcreteBooleanState, Seq[Int])] = {
    simulateWithTimestamps(initialStates, applyAllFunctionSubsets(functions))
  }

  def simulateWithTimestamps(
    initialStates: Set[ConcreteBooleanState],
    stateTransitionFunction: ConcreteBooleanState => Set[ConcreteBooleanState]
  ): Set[(ConcreteBooleanState, Seq[Int])] = {
    var stateToTimestamps = Map[ConcreteBooleanState, Seq[Int]]()

    var currentStates = Set.empty[ConcreteBooleanState]
    var nextStates = initialStates

    var step = 0
    while (currentStates != nextStates && step < SIMULATION_DEPTH_LIMIT) {
      currentStates = nextStates

      for (state <- currentStates) {
        stateToTimestamps = MapUtil.addMultisetBinding(stateToTimestamps,
          state, step)
      }

      nextStates = currentStates flatMap stateTransitionFunction

      step += 1
    }

    stateToTimestamps.toSet
  }

  def simulateOneStepWithStateGraph(
    functions: Map[String, FunExpr],
    initialStates: Set[ConcreteBooleanState]
  ): DirectedBooleanStateGraph = {
    simulateWithStateGraph(initialStates, applyFunctionAlternatives(functions))
  }

  def simulateAnyStepsWithStateGraph(
    functions: Map[String, FunExpr],
    initialStates: Set[ConcreteBooleanState]
  ): DirectedBooleanStateGraph = {
    simulateWithStateGraph(initialStates, applyAllFunctionSubsets(functions))
  }

  def simulateWithStateGraph(
    initialStates: Set[ConcreteBooleanState],
    stateTransitionFunction: ConcreteBooleanState => Set[ConcreteBooleanState]
  ): DirectedBooleanStateGraph = {
    val nodeCounter = new UniqueCounter
    val measurementCounter = new UniqueCounter

    var stateToNode = Map[ConcreteBooleanState, StateGraphVertex]()

    def makeNodeId(): String = "v" + nodeCounter.next
    def makeMeasurementId(): String = "m" + measurementCounter.next

    def makeMeasurement(s: ConcreteBooleanState): Measurement[Boolean] = {
      Measurement(makeMeasurementId(), s)
    }

    def makeNodeForState(s: ConcreteBooleanState): StateGraphVertex = {
      StateGraphVertex(makeNodeId(), s, Seq(makeMeasurement(s)))
    }

    def getNodeForState(s: ConcreteBooleanState): StateGraphVertex = {
      stateToNode.get(s) match {
        case Some(n) => n
        case None => {
          val newNode = makeNodeForState(s)
          stateToNode += s -> newNode
          newNode
        }
      }
    }

    var stateGraph = UnlabeledDiGraph[StateGraphVertex]()
    var currentNodes = Set.empty[StateGraphVertex]
    var nextNodes = Set.empty[StateGraphVertex]

    for (is <- initialStates) {
      val node = getNodeForState(is)
      stateGraph = stateGraph.addVertex(node)
      nextNodes += node
    }

    var step = 0
    while (currentNodes != nextNodes && step < SIMULATION_DEPTH_LIMIT) {
      currentNodes = nextNodes
      nextNodes = Set.empty

      for (srcNode <- currentNodes) {
        val targetStates = stateTransitionFunction(srcNode.state)
        val targetNodes = targetStates map getNodeForState

        for (tgtNode <- targetNodes) {
          nextNodes += tgtNode
          stateGraph = stateGraph.addVertex(tgtNode)
          stateGraph = stateGraph.addEdge(srcNode, tgtNode)
        }
      }

      step += 1
    }

    stateGraph
  }

  def stateIsFixpoint(
    functions: Map[String, FunExpr],
    state: ConcreteBooleanState
  ): Boolean = {
    functions forall {
      case (label, expr) => {
        updatedState(label, expr, state) == state
      }
    }
  }

  private def updatedStateIfChanged(
    label: String,
    fun: FunExpr,
    inputState: ConcreteBooleanState
  ): Option[ConcreteBooleanState] = {
    val updated = updatedState(label, fun, inputState)
    if (updated != inputState) {
      Some(updated)
    } else {
      None
    }
  }

  private def functionChangesState(
    label: String,
    fun: FunExpr,
    inputState: ConcreteBooleanState
  ): Boolean = {
    val updated = updatedState(label, fun, inputState)
    updated != inputState
  }

  private def updatedState(
    label: String,
    fun: FunExpr,
    inputState: ConcreteBooleanState
  ): ConcreteBooleanState = {
    val funOutput = FunctionTrees.eval(fun, inputState)
    inputState.replaceValue(label, funOutput)
  }

}
