package karme.graphs

import karme.CellTrajectories.CellTrajectory
import karme.Experiments
import karme.Experiments.BooleanExperiment
import karme.Experiments.BooleanMeasurement
import karme.Experiments.Experiment
import karme.Experiments.Measurement
import karme.Experiments.ThreeValued
import karme.Experiments.ThreeValuedMeasurement
import karme.Experiments.ThreeValuedExperiment
import karme.discretization.Discretization
import karme.graphs.Graphs._
import karme.transformations.DiscreteStateAnalysis
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.synthesis.Transitions.ThreeValuedState
import karme.util.MathUtil

import scala.collection.mutable

object StateGraphs {

  type UndirectedBooleanStateGraph = UnlabeledGraph[StateGraphVertex]
  type DirectedBooleanStateGraph = UnlabeledDiGraph[StateGraphVertex]

  def fromDiscreteExperiment(
    booleanExperiment: BooleanExperiment,
    maxHammingDistance: Int
  ): UndirectedBooleanStateGraph = {
    val stateToMeasurements = booleanExperiment.measurements.groupBy(_.values)

    val V = stateToMeasurements map {
      case (state, ms) =>
        val booleanState = ConcreteBooleanState(
          booleanExperiment.names.zip(state).toMap)
        StateGraphVertex(booleanState, ms)
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

  def fromThreeValuedExperiment(
    threeValuedExperiment: ThreeValuedExperiment,
    maxHammingDistance: Int
  ): UndirectedBooleanStateGraph = {
    // first expand to Boolean experiment
    val booleanExperiment = threeValuedExperimentToBoolean(
      threeValuedExperiment)

    // then build a N-Hamming graph using above function
    // try out 1-Hamming to see if it's connected and for simple analysis
    val hammingGraph = fromDiscreteExperiment(booleanExperiment,
      maxHammingDistance)

    // TODO replace >1-Hamming edges with expanded 1-Hamming paths
    hammingGraph
  }

  private def threeValuedExperimentToBoolean(
    threeValuedExperiment: ThreeValuedExperiment
  ): BooleanExperiment = {
    val booleanMeasurements = threeValuedExperiment.measurements.flatMap{
      measurement => {
        val booleanStates = expandThreeValuedState(measurement.values)
        booleanStates map { booleanState =>
          Measurement[Boolean](measurement.id, booleanState)
        }
      }
    }

    Experiment(threeValuedExperiment.names, booleanMeasurements)
  }

  private def expandThreeValuedState(
    state: Seq[ThreeValued]
  ): Set[List[Boolean]] = {
    // compute Set of booleans for each
    val booleanSets = state map Experiments.threeValuedToBooleanSet

    // take cartesian product of set
    MathUtil.cartesianProduct(booleanSets.toList)
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
      val directionMaps = trajectories map (t => trajectoryDirections(g, t))

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

    private def trajectoryDirections(
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
    trajectories.map{ trajectory =>
      val nodePseudotimePairs = vertices.map{ v =>
        v -> avgNodePseudotime(v, trajectory)
      }.filter(_._2.isDefined)
      val firstV = nodePseudotimePairs.toList.sortBy(_._2).head._1
      firstV.state
    }.toSet
  }

  def avgNodePseudotime(
    node: StateGraphVertex, trajectory: CellTrajectory
  ): Option[Double] = {
    val nodeCellIDs = node.measurements.map(_.id)
    val pseudotimes = nodeCellIDs collect {
      case id if trajectory.isDefinedAt(id) => trajectory(id)
    }
    if (pseudotimes.isEmpty) {
      None
    } else {
      Some(MathUtil.mean(pseudotimes))
    }
  }


  def nodeMeasurementsPerCluster(
    n: StateGraphVertex, clustering: mutable.MultiMap[String, String]
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
