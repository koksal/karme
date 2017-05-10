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

import scala.collection.mutable

object StateGraphs {

  type UndirectedBooleanStateGraph = UnlabeledGraph[StateGraphVertex]
  type DirectedBooleanStateGraph = UnlabeledDiGraph[StateGraphVertex]

  def namesFromStateGraph(g: GraphLike[StateGraphVertex, _, _]): Seq[String] = {
    g.V.headOption match {
      case Some(v) => v.names
      case None => sys.error("Names in empty graph.")
    }
  }

  /**
    * Builds a state graph by adding all edges within maximum Hamming distance.
    */
  def fromBooleanExperiment(
    booleanExperiment: BooleanExperiment,
    maxHammingDistance: Int
  ): UndirectedBooleanStateGraph = {
    val stateToMeasurements = booleanExperiment.measurements.groupBy(_.state)

    val V = stateToMeasurements map {
      case (state, ms) =>
        StateGraphVertex(state, ms)
    }

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
    state: ConcreteBooleanState,
    measurements: Seq[BooleanMeasurement]
  ) extends Ordered[StateGraphVertex] {
    override def compare(o: StateGraphVertex): Int = {
      assert(this.state.size == o.state.size)

      import scala.math.Ordering.Implicits._
      if (this.state.orderedValues < o.state.orderedValues) {
        -1
      } else if (this.state == o.state) {
        0
      } else {
        1
      }
    }

    def names: Seq[String] = {
      state.orderedKeys
    }
  }

  case class ThreeValuedStateGraphVertex(
    state: ThreeValuedState,
    measurements: Seq[ThreeValuedMeasurement]
  ) extends Ordered[ThreeValuedStateGraphVertex] {
    override def compare(o: ThreeValuedStateGraphVertex): Int = {

      implicit val ordering = ThreeValuedState.threeValuedOrdering
      import scala.math.Ordering.Implicits._

      if (this.state.orderedValues < o.state.orderedValues) {
        -1
      } else if (this.state == o.state) {
        0
      } else {
        1
      }
    }
  }

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
      val directions = new mutable.HashMap[UnlabeledEdge[StateGraphVertex],
        mutable.Set[EdgeDirection]]() with
        mutable.MultiMap[UnlabeledEdge[StateGraphVertex], EdgeDirection]

      // compute all directions that can be assigned with trajectories
      val directionMaps = trajectories map (t =>
        orientByTrajectoryRankSum(g, t))

      // merge directions
      for (edge <- g.E) {
        val directionSets = directionMaps collect {
          case dm if dm.isDefinedAt(edge) => dm(edge)
        }
        for (ds <- directionSets; d <- ds) {
          directions.addBinding(edge, d)
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

    private def orientByTrajectoryAvg(
      g: UndirectedBooleanStateGraph,
      trajectory: CellTrajectory
    ): Map[UnlabeledEdge[StateGraphVertex], Set[EdgeDirection]] = {
      var res = Map[UnlabeledEdge[StateGraphVertex], Set[EdgeDirection]]()

      // for each state, compute average pseudotime for given trajectory
      var nodeToPseudotime = Map[StateGraphVertex, Double]()
      for (node <- g.V) {
        avgNodePseudotime(node, trajectory) match {
          case Some(pt) => nodeToPseudotime += node -> pt
          case None =>
        }
      }

      // for each edge, assign a direction if possible.
      for (e <- g.E) {
        (nodeToPseudotime.get(e.v1), nodeToPseudotime.get(e.v2)) match {
          case (Some(pt1), Some(pt2)) => {
            if (pt1 < pt2) {
              res += e -> Set(Forward)
            } else if (pt1 > pt2) {
              res += e -> Set(Backward)
            } else {
              println("Average pseudotime equal between nodes.")
              res += e -> Set(Forward, Backward)
            }
          }
          case _ =>
        }
      }

      res
    }

  }

  def initialTrajectoryStates(
    vertices: Set[StateGraphVertex],
    trajectories: Iterable[CellTrajectory]
  ): Set[ConcreteBooleanState] = {
    val initialStateOpts = trajectories map (t =>
      initialTrajectoryState(vertices, t))

    initialStateOpts.flatten.toSet
  }

  def initialTrajectoryState(
    vertices: Set[StateGraphVertex],
    trajectory: CellTrajectory
  ): Option[ConcreteBooleanState] = {
    val nodePseudotimePairs = vertices.map{ v =>
      v -> avgNodePseudotime(v, trajectory)
    }.filter(_._2.isDefined)
    nodePseudotimePairs.toList.sortBy(_._2).headOption.map(_._1.state)
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


  def makeNodeIDs(
    vs: Iterable[StateGraphVertex]
  ): Map[StateGraphVertex, String] = {
    vs.toSeq.sorted.zipWithIndex.map{
      case (v, i) => {
        v -> s"V$i"
      }
    }.toMap
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
