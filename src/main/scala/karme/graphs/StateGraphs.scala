package karme.graphs

import karme.CellTrajectories.CellTrajectory
import karme.Experiments.{DiscreteExperiment, DiscreteMeasurement}
import karme.graphs.Graphs.EdgeLike
import karme.graphs.Graphs.GraphLike
import karme.transformations.DiscreteStateAnalysis
import karme.graphs.Graphs.{Backward, EdgeDirection, Forward}
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.util.MathUtil

import scala.collection.mutable

object StateGraphs {

  def fromDiscreteExperiment(
    discreteExperiment: DiscreteExperiment,
    maxHammingDistance: Int
  ): UndirectedStateGraph = {
    val stateToMeasurements = discreteExperiment.measurements.groupBy(_.values)

    val V = stateToMeasurements map {
      case (state, ms) => DiscreteStateGraphNode(state, ms)
    }

    var g = new UndirectedStateGraph(discreteExperiment.names)

    // Add edges with Hamming distance <= max
    val vSeq = V.toIndexedSeq
    for {
      i <- 0 until vSeq.size
      j <- (i + 1) until vSeq.size
    } {
      val v1 = vSeq(i)
      val v2 = vSeq(j)

      val dist = DiscreteStateAnalysis.distance(v1.state, v2.state)
      if (dist <= maxHammingDistance) {
        g = g.addEdge(v1, v2)
      }
    }

    g
  }

  trait StateGraphLike[Vertex <: Ordered[Vertex], Edge <: EdgeLike[Vertex]] {
    this: GraphLike[Vertex, Edge] =>

    def names: Seq[String]

    def edgeLabels
  }

  case class StateGraphVertex(
    state: ConcreteBooleanState,
    measurements: Seq[DiscreteMeasurement]
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

  case class StateGraphEdge(v1: StateGraphVertex, v2: StateGraphVertex)
    extends EdgeLike[StateGraphVertex] {

    def labels: Seq[String] = {
      val names = v1.state.orderedKeys
      DiscreteStateAnalysis.nonIdenticalNames(names, v1.state.orderedValues,
        v2.state.orderedValues)
    }
  }

  class UndirectedStateGraph(val names: Seq[String]) extends UndirectedGraph {
    type Vertex = DiscreteStateGraphNode
    type Edge = DiscreteStateGraphEdge

    def addVertex(v: DiscreteStateGraphNode): UndirectedStateGraph = {
      new UndirectedStateGraph(names) {
        val V: Set[DiscreteStateGraphNode] = this.V + v
        val E: Set[DiscreteStateGraphEdge] = E
      }
    }

    def addEdge(
      v1: DiscreteStateGraphNode, v2: DiscreteStateGraphNode
    ): UndirectedStateGraph = {
      val newEdge = if (v1 < v2) {
        DiscreteStateGraphEdge(v1, v2)
      } else {
        DiscreteStateGraphEdge(v2, v1)
      }

      new UndirectedStateGraph(V + v1 + v2, E + newEdge, names)
      new UndirectedStateGraph(names) {

      }
      this.addVertex(v1).addVertex(v2)
    }

    def orientByTrajectories(
      trajectories: Seq[CellTrajectory]
    ): DirectedStateGraph = {
      val directions = new mutable.HashMap[DiscreteStateGraphEdge,
        mutable.Set[EdgeDirection]]() with
        mutable.MultiMap[DiscreteStateGraphEdge, EdgeDirection]

      // compute all directions that can be assigned with trajectories
      val directionMaps = trajectories map trajectoryDirections

      // check that inferred directions are not contradictory & merge directions
      for (edge <- E) {
        val ds = directionMaps collect {
          case dm if dm.isDefinedAt(edge) => dm(edge)
        }
        // this assertion does not hold
        // assert(ds.distinct.size == 1)
        for (d <- ds) {
          directions.addBinding(edge, d)
        }
      }

      // we filter the graph down to edges that could be oriented
      new DirectedStateGraph(V, directions.keySet.toSet, directions, names)
    }

    private def trajectoryDirections(
      trajectory: CellTrajectory
    ): Map[DiscreteStateGraphEdge, EdgeDirection] = {
      var res = Map[DiscreteStateGraphEdge, EdgeDirection]()

      // for each state, compute average pseudotime for given trajectory
      var nodeToPseudotime = Map[DiscreteStateGraphNode, Double]()
      for (node <- V) {
        avgNodePseudotime(node, trajectory) match {
          case Some(pt) => nodeToPseudotime += node -> pt
          case None =>
        }
      }

      // for each edge, assign a direction if possible.
      for (edge @ DiscreteStateGraphEdge(n1, n2) <- E) {
        (nodeToPseudotime.get(n1), nodeToPseudotime.get(n2)) match {
          case (Some(pt1), Some(pt2)) => {
            val dir = if (pt1 < pt2) Forward else Backward
            res += edge -> dir
          }
          case _ =>
        }
      }

      res
    }

    private def avgNodePseudotime(
      node: DiscreteStateGraphNode, trajectory: CellTrajectory
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

  }

  class DirectedStateGraph(
    override val V: Set[DiscreteStateGraphNode],
    override val E: Set[DiscreteStateGraphEdge],
    val edgeDirections: mutable.MultiMap[DiscreteStateGraphEdge, EdgeDirection],
    override val names: Seq[String]
  ) extends UndirectedStateGraph(V, E, names) with DirectedGraph {

    override def addEdge(
      v1: DiscreteStateGraphNode, v2: DiscreteStateGraphNode
    ): DirectedStateGraph = {
      val newEdge = if (v1 < v2) {
        DiscreteStateGraphEdge(v1, v2)
      } else {
        DiscreteStateGraphEdge(v2, v1)
      }

      val newDir = if (v1 < v2) Forward else Backward

      new DirectedStateGraph(V + v1 + v2, E + newEdge,
        edgeDirections.addBinding(newEdge, newDir), names)
    }

  }

  def nodeMeasurementsPerCluster(
    n: DiscreteStateGraphNode, clustering: mutable.MultiMap[String, String]
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

}
