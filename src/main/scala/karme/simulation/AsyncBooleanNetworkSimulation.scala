package karme.simulation

import karme.Experiments.Measurement
import karme.graphs.Graphs.UnlabeledDiGraph
import karme.graphs.StateGraphs.{DirectedBooleanStateGraph, StateGraphVertex}
import karme.synthesis.FunctionTrees
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.util.{MapUtil, UniqueCounter}

object AsyncBooleanNetworkSimulation {

  val SIMULATION_DEPTH_LIMIT = 100

  def simulate(
    functions: Map[String, FunExpr],
    initialStates: Set[ConcreteBooleanState]
  ): Set[ConcreteBooleanState] = {
    simulateWithTimestamps(functions, initialStates).map(_._1)
  }

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
        functions flatMap {
          case (label, fun) => {
            updatedStateIfChanged(label, fun, s)
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
            targetStates map (s => getNodeForState(s))
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

    if (currentNodes != nextNodes) {
      println("Fixpoint not reached in simulation.")
    } else {
      println(s"Fixpoint reached in $step steps in simulation.")
    }

    stateGraph
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
