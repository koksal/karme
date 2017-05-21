package karme.graphs

import karme.CellTrajectories.CellTrajectory
import karme.Experiments
import karme.Experiments.BooleanExperiment
import karme.Experiments.BooleanMeasurement
import karme.Experiments.Experiment
import karme.Experiments.High
import karme.Experiments.Measurement
import karme.Experiments.ThreeValued
import karme.Experiments.ThreeValuedMeasurement
import karme.Experiments.ThreeValuedExperiment
import karme.Experiments.Uncertain
import karme.analysis.DiscreteStateAnalysis
import karme.evaluation.RankSumTest
import karme.graphs.Graphs._
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.synthesis.Transitions.GenericState
import karme.synthesis.Transitions.ThreeValuedState
import karme.util.MapUtil
import karme.util.MathUtil
import karme.util.UniqueCounter

import scala.collection.mutable

object StateGraphs {

  type UndirectedBooleanStateGraph = UnlabeledGraph[StateGraphVertex]
  type DirectedBooleanStateGraph = UnlabeledDiGraph[StateGraphVertex]

  def namesFromStateGraph(g: GraphLike[StateGraphVertex, _, _]): Set[String] = {
    g.V.headOption match {
      case Some(v) => v.names.toSet
      case None => Set.empty
    }
  }

  /**
    * Builds a state graph by adding all edges within maximum Hamming distance.
    */
  def fromBooleanExperiment(
    booleanExperiment: BooleanExperiment,
    maxHammingDistance: Int
  ): UndirectedBooleanStateGraph = {
    val V = nodesFromExperiment(booleanExperiment)

    var g = new UndirectedBooleanStateGraph(V = V.toSet)

    // Add edges with Hamming distance <= max
    val vSeq = V.toIndexedSeq
    for {
      i <- 0 until vSeq.size
      j <- (i + 1) until vSeq.size
    } {
      val v1 = vSeq(i)
      val v2 = vSeq(j)

      val dist = DiscreteStateAnalysis.hammingDistance(v1.state, v2.state)
      if (dist <= maxHammingDistance) {
        g = g.addEdge(v1, v2)
      }
    }

    g
  }

  def nodesFromExperiment(
    booleanExperiment: BooleanExperiment
  ): Set[StateGraphVertex] = {
    val stateToMeasurements = booleanExperiment.measurements.groupBy(_.state)

    val ctr = new UniqueCounter()

    val V = stateToMeasurements map {
      case (state, ms) =>
        StateGraphVertex(s"v_${ctr.next}", state, ms)
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
    * Filters out every state with uncertain values and converts remaining
    * states to Boolean states.
    */
  def eliminateStatesWithUncertainValues(
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

  object StateGraphOps {
    def names(g: GraphLike[StateGraphVertex, _, _]): Seq[String] = {
      g.V.head.state.orderedKeys
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
      var directions =
        Map[UnlabeledEdge[StateGraphVertex], Set[EdgeDirection]]()

      // compute all directions that can be assigned with trajectories
      val directionMaps = trajectories map (t =>
        orientByTrajectoryRankSum(g, t))

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

    private def orientByTrajectoryRankSum(
      g: UndirectedBooleanStateGraph,
      trajectory: CellTrajectory
    ): Map[UnlabeledEdge[StateGraphVertex], Set[EdgeDirection]] = {
      var res = Map[UnlabeledEdge[StateGraphVertex], Set[EdgeDirection]]()

      for (e @ UnlabeledEdge(v1, v2) <- g.E) {
        val dirOpt = orientByRankSum(
          nodePseudotimes(v1, trajectory),
          nodePseudotimes(v2, trajectory)
        )
        dirOpt foreach {
          res += e -> Set(_)
        }
      }

      res
    }

    def orientByRankSum(
      leftPseudotimes: Seq[Double], rightPseudotimes: Seq[Double]
    ): Option[EdgeDirection] = {
      val P_VALUE_THRESHOLD = 0.05

      if (leftPseudotimes.isEmpty || rightPseudotimes.isEmpty) {
        None
      } else {
        val forwardPVal = new RankSumTest(
          rightPseudotimes, leftPseudotimes).run().pValue
        val backwardPVal = new RankSumTest(
          leftPseudotimes, rightPseudotimes).run().pValue

        if (forwardPVal <= P_VALUE_THRESHOLD) {
          assert(backwardPVal > P_VALUE_THRESHOLD)
          Some(Forward)
        } else if (backwardPVal <= P_VALUE_THRESHOLD) {
          Some(Backward)
        } else {
          None
        }
      }
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
