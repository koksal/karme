package karme.simulation

import karme.CellTrajectories.CellTrajectory
import karme.Experiments.Measurement
import karme.evaluation.synthetic.FixpointStates
import karme.graphs.Graphs.UnlabeledDiGraph
import karme.graphs.StateGraphs.{DirectedBooleanStateGraph, StateGraphVertex}
import karme.synthesis.FunctionTrees
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.util.{CollectionUtil, MapUtil, TimingUtil, UniqueCounter}

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

  private def simulateWithTimestamps(
    initialStates: Set[ConcreteBooleanState],
    stateTransitionFunction: ConcreteBooleanState => Set[ConcreteBooleanState]
  ): Set[(ConcreteBooleanState, Seq[Int])] = {
    TimingUtil.time("timestamp-generating simulation") {
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
  }

  /**
    * Computes the trimmed state graph.
    *
    * A trimmed state graph is one in which every path from the initial
    * states to a node is of the same length, and if a node is included in
    * the graph, it must eventually reach a fixed point state.
    */
  def simulateOneStepWithTrimmedStateGraph(
    functions: Map[String, FunExpr],
    initialStates: Set[ConcreteBooleanState]
  ): (DirectedBooleanStateGraph, CellTrajectory) = {
    val (simulationGraph, trajectory) = simulateWithStateGraph(
      initialStates,
      applyFunctionAlternatives(functions),
      allowDifferentArrivalTimes = false
    )

    val trimmedGraph = trimGraph(simulationGraph, functions)

    (
      trimmedGraph,
      filterTrajectoryForGraph(trajectory, trimmedGraph)
    )
  }

  def simulateOneStepWithStateGraph(
    functions: Map[String, FunExpr],
    initialStates: Set[ConcreteBooleanState],
    allowDifferentArrivalTimes: Boolean = true
  ): (DirectedBooleanStateGraph, CellTrajectory) = {
    simulateWithStateGraph(
      initialStates,
      applyFunctionAlternatives(functions),
      allowDifferentArrivalTimes
    )
  }

  def simulateAnyStepsWithStateGraph(
    functions: Map[String, FunExpr],
    initialStates: Set[ConcreteBooleanState],
    allowDifferentArrivalTimes: Boolean = true
  ): (DirectedBooleanStateGraph, CellTrajectory) = {
    simulateWithStateGraph(
      initialStates,
      applyAllFunctionSubsets(functions),
      allowDifferentArrivalTimes
    )
  }

  private def simulateWithStateGraph(
    initialStates: Set[ConcreteBooleanState],
    stateTransitionFunction: ConcreteBooleanState => Set[ConcreteBooleanState],
    allowDifferentArrivalTimes: Boolean
  ): (DirectedBooleanStateGraph, CellTrajectory) = {
    val nodeCounter = new UniqueCounter
    val measurementCounter = new UniqueCounter

    var stateToMeasurements =
      Map[ConcreteBooleanState, Set[Measurement[Boolean]]]()
    var stateToTargetStates =
      Map[ConcreteBooleanState, Set[ConcreteBooleanState]]()
    var measurementIDToTimestamp = Map[String, Double]()

    def makeNodeId(): String = "v" + nodeCounter.next
    def makeMeasurementId(): String = "m" + measurementCounter.next

    def makeMeasurement(s: ConcreteBooleanState): Measurement[Boolean] = {
      Measurement(makeMeasurementId(), s)
    }

    def addMeasurement(s: ConcreteBooleanState, t: Int) = {
      val m = makeMeasurement(s)
      stateToMeasurements = MapUtil.addBinding(stateToMeasurements, s, m)
      measurementIDToTimestamp += m.id -> t
    }

    def addStateEdge(
      src: ConcreteBooleanState, tgt: ConcreteBooleanState
    ) = {
      stateToTargetStates = MapUtil.addBinding(stateToTargetStates, src, tgt)
    }

    var step = 0

    var currentStates = Set.empty[ConcreteBooleanState]
    var nextStates = initialStates

    while (currentStates != nextStates && step < SIMULATION_DEPTH_LIMIT) {
      currentStates = nextStates
      nextStates = Set.empty

      for (state <- currentStates) {
        addMeasurement(state, step)
      }

      for (srcState <- currentStates) {
        val targetStates = stateTransitionFunction(srcState)

        for (tgtState <- targetStates) {
          val targetNotReachedInPriorStep =
            !stateToMeasurements.isDefinedAt(tgtState)

          if (allowDifferentArrivalTimes || targetNotReachedInPriorStep) {
            nextStates += tgtState
            addStateEdge(srcState, tgtState)
          }
        }
      }

      step += 1
    }

    var stateGraph = UnlabeledDiGraph[StateGraphVertex]()
    var stateToVertex = Map[ConcreteBooleanState, StateGraphVertex]()
    for ((state, ms) <- stateToMeasurements) {
      val v = StateGraphVertex(makeNodeId(), state, ms.toSeq)
      stateGraph = stateGraph.addVertex(v)
      stateToVertex += state -> v
    }

    for ((srcState, tgtStates) <- stateToTargetStates) {
      for (tgtState <- tgtStates) {
        stateGraph = stateGraph.addEdge(
          stateToVertex(srcState), stateToVertex(tgtState))
      }
    }

    (stateGraph, measurementIDToTimestamp)
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

  private def removeStatesNotReachingFixpoints(
    simulationGraph: DirectedBooleanStateGraph,
    allowedFixpoints: Set[ConcreteBooleanState]
  ) = {
    val allowedFixpointNodes = allowedFixpoints.flatMap(
      s => simulationGraph.V.filter(v => v.state == s))

    val reverseSimulationGraph = simulationGraph.reverse()

    val reachableFromFinalNodes = allowedFixpointNodes flatMap { n =>
      reverseSimulationGraph.shortestPaths(n).map(_.last)
    }

    val nonReachableFromFinal = simulationGraph.V -- reachableFromFinalNodes

    var resultGraph = simulationGraph

    for (n <- nonReachableFromFinal) {
      resultGraph = resultGraph.removeVertex(n)
    }

    resultGraph
  }

  private def trimGraph(
    graph: DirectedBooleanStateGraph,
    functions: Map[String, FunExpr]
  ): DirectedBooleanStateGraph = {

    val simulationStates = graph.V.map(_.state)
    val fixpointStates = simulationStates filter { s =>
      AsyncBooleanNetworkSimulation.stateIsFixpoint(functions, s)
    }

    removeStatesNotReachingFixpoints(graph, fixpointStates)
  }

  private def filterTrajectoryForGraph(
    trajectory: CellTrajectory,
    graph: DirectedBooleanStateGraph
  ): CellTrajectory = {
    val msIDs = graph.V.flatMap(v => v.measurements.map(_.id))

    trajectory filter { case (id, _) => msIDs.contains(id) }
  }

}
