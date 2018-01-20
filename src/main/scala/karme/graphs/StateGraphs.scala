package karme.graphs

import karme.CellTrajectories.CellTrajectory
import karme.Experiments
import karme.Experiments._
import karme.analysis.DiscreteStateAnalysis
import karme.graphs.Graphs._
import karme.synthesis.Transitions.{ConcreteBooleanState, GenericState, ThreeValuedState}
import karme.util.{MapUtil, MathUtil, UniqueCounter}

import scala.collection.mutable

object StateGraphs {

  case class StateGraphVertex(
    id: String,
    state: ConcreteBooleanState,
    measurements: Seq[BooleanMeasurement]
  ) extends VertexLike {
    def names: Seq[String] = {
      state.orderedKeys
    }
  }

  case class ThreeValuedStateGraphVertex(
    id: String,
    state: ThreeValuedState,
    measurements: Seq[ThreeValuedMeasurement]
  ) extends VertexLike

  type UndirectedBooleanStateGraph = UnlabeledGraph[StateGraphVertex]
  type DirectedBooleanStateGraph = UnlabeledDiGraph[StateGraphVertex]

  private val stateGraphVertexCounter = new UniqueCounter()

  def makeNode(
    state: ConcreteBooleanState, measurements: Seq[BooleanMeasurement]
  ): StateGraphVertex = {
    StateGraphVertex(s"v${stateGraphVertexCounter.next}", state, measurements)
  }

  def namesFromStateGraph(g: GraphLike[StateGraphVertex, _, _]): Set[String] = {
    g.V.headOption match {
      case Some(v) => v.names.toSet
      case None => Set.empty
    }
  }

  def nodesFromExperiment(
    booleanExperiment: BooleanExperiment
  ): Set[StateGraphVertex] = {
    val stateToMeasurements = booleanExperiment.measurements.groupBy(_.state)

    val V = stateToMeasurements map {
      case (state, ms) => makeNode(state, ms)
    }

    V.toSet
  }

  /**
    * Converts every state with uncertain values to a set of states with all
    * Boolean combinations of values.
    */
  def expandWithBooleanCombinations(
    threeValuedExperiment: ThreeValuedExperiment
  ): BooleanExperiment = {
    val booleanMeasurements = threeValuedExperiment.measurements.flatMap{
      measurement => {
        val booleanStates = expandThreeValuedState(measurement.state)
        booleanStates map { booleanState =>
          Measurement[Boolean](measurement.id, booleanState)
        }
      }
    }

    Experiment(booleanMeasurements)
  }

  /**
    * Filters out every measurement with uncertain values and converts
    * associated states to Boolean states.
    */
  def removeMeasurementsWithUncertainDiscretization(
    threeValuedExperiment: ThreeValuedExperiment
  ): BooleanExperiment = {
    // filter out measurements which have states with uncertain values.
    val msWithoutUncertainty = threeValuedExperiment.measurements.filter { m =>
      m.state.orderedValues.forall(_ != Uncertain)
    }

    // map to measurements with Boolean states
    val booleanMs = msWithoutUncertainty map { m =>
      val boolState = m.state.mapValues { tv =>
        assert(tv != Uncertain)
        tv == High
      }
      Measurement(m.id, boolState)
    }
    Experiment(booleanMs)
  }

  private def expandThreeValuedState(
    state: GenericState[ThreeValued]
  ): Set[GenericState[Boolean]] = {
    // compute Set of booleans for each
    val booleanSets =
      state.orderedValues map Experiments.threeValuedToBooleanSet

    // take cartesian product of set
    val booleanSeqs = MathUtil.cartesianProduct(booleanSets.toList)
    booleanSeqs map { bs =>
      GenericState(state.orderedKeys.zip(bs).toMap)
    }
  }

  object UndirectedStateGraphOps {
    def edgeLabels(e: UnlabeledEdge[StateGraphVertex]): Seq[String] = {
      val names = e.v1.state.orderedKeys
      DiscreteStateAnalysis.nonIdenticalNames(names, e.v1.state.orderedValues,
        e.v2.state.orderedValues)
    }

    def orientByTrajectories(
      g: UndirectedBooleanStateGraph,
      trajectories: Seq[CellTrajectory]
    ): DirectedBooleanStateGraph = {
      val graphOrienter = new GraphOrientationByTrajectory(g)

      var directions =
        Map[UnlabeledEdge[StateGraphVertex], Set[EdgeDirection]]()

      // compute all directions that can be assigned with trajectories
      val directionMaps = trajectories map (t =>
        graphOrienter.orientForTrajectory(t))

      // merge directions
      for (edge <- g.E) {
        val directionSets = directionMaps collect {
          case dm if dm.isDefinedAt(edge) => dm(edge)
        }
        for (ds <- directionSets; d <- ds) {
          directions = MapUtil.addBinding(directions, edge, d)
        }
      }

      // we filter the graph down to edges that could be oriented
      new DirectedBooleanStateGraph(g.V, directions.keySet.toSet, directions)
    }
  }

  def avgNodePseudotime(
    node: StateGraphVertex, trajectory: CellTrajectory
  ): Option[Double] = {
    val pseudotimes = nodePseudotimes(node, trajectory)
    if (pseudotimes.isEmpty) {
      None
    } else {
      Some(MathUtil.mean(pseudotimes))
    }
  }

  def nodePseudotimes(
    node: StateGraphVertex, trajectory: CellTrajectory
  ): Seq[Double] = {
    val nodeCellIDs = node.measurements.map(_.id)
    nodeCellIDs collect {
      case id if trajectory.isDefinedAt(id) => trajectory(id)
    }
  }

  def nodeMeasurementsPerCluster(
    n: StateGraphVertex, clustering: Map[String, Set[String]]
  ): mutable.MultiMap[String, String] = {
    val result = new mutable.HashMap[String, mutable.Set[String]]()
      with mutable.MultiMap[String, String]
    val nodeMeasurementIDs = n.measurements.map(_.id)
    for ((clusterName, cellIDs) <- clustering) {
      val commonIDs = nodeMeasurementIDs.toSet.intersect(cellIDs)
      for (id <- commonIDs) {
        result.addBinding(clusterName, id)
      }
    }
    result
  }

  def makeCellIDs(
    nodeToID: Map[StateGraphVertex, String]
  ): Map[String, String] = {
    val cellIDs = nodeToID flatMap {
      case (node, id) => node.measurements.map(m => m.id -> id)
    }

    cellIDs
  }
}
