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

  def simulate(
    functions: Map[String, FunExpr],
    initialStates: Set[ConcreteBooleanState]
  ): Set[ConcreteBooleanState] = {
    simulateOneStepWithTimestamps(functions, initialStates).map(_._1)
  }

  def simulateOneStepWithTimestamps(
    functions: Map[String, FunExpr],
    initialStates: Set[ConcreteBooleanState]
  ): Set[(ConcreteBooleanState, Seq[Int])] = {
    def applyFunctionAlternatives(
      s: ConcreteBooleanState
    ): Set[ConcreteBooleanState] = {
      functions.flatMap{
        case (label, fun) => {
          updatedStateIfChanged(label, fun, s)
        }
      }.toSet
    }

    simulateWithTimestamps(functions, initialStates, applyFunctionAlternatives)
  }

  def simulateAnyStepsWithTimestamps(
    functions: Map[String, FunExpr],
    initialStates: Set[ConcreteBooleanState]
  ): Set[(ConcreteBooleanState, Seq[Int])] = {
    def applyAllFunctionSubsets(
      s: ConcreteBooleanState
    ): Set[ConcreteBooleanState] = {
      val functionsThatChangeInputState = functions filter {
        case (label, fun) => functionChangesState(label, fun, s)
      }

      CollectionUtil.nonEmptySubsets(functionsThatChangeInputState.toSet) map {
        functionSubset => {
          functionSubset.foldLeft(s) {
            case (acc, (label, fun)) => {
              updatedState(label, fun, acc)
            }
          }
        }

      }
    }

    simulateWithTimestamps(functions, initialStates, applyAllFunctionSubsets)
  }

  def simulateWithTimestamps(
    functions: Map[String, FunExpr],
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

//      nextStates = currentStates flatMap { s =>
//        functions flatMap {
//          case (label, fun) => {
//            updatedStateIfChanged(label, fun, s)
//          }
//        }
//      }
      nextStates = currentStates flatMap stateTransitionFunction

      step += 1
    }

    stateToTimestamps.toSet
  }

  def simulateWithStateGraph(
    functions: Map[String, FunExpr],
    initialStates: Set[ConcreteBooleanState]
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
        val targetNodes = functions flatMap {
          case (label, fun) => {
            val targetStates = updatedStateIfChanged(label, fun, srcNode.state)
            targetStates map getNodeForState
          }
        }

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
    val newMapping = inputState.mapping.updated(label, funOutput)
    new ConcreteBooleanState(newMapping)
  }

}
