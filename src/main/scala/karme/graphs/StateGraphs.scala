package karme.graphs

import karme.Experiments.{DiscreteExperiment, DiscreteMeasurement, Measurement}
import karme.analysis.DiscreteStateAnalysis
import karme.graphs.Graphs.{Backward, EdgeDirection, Forward, UndirectedGraph}

import scala.collection.mutable

object StateGraphs {

  def fromDiscreteExperiment(
    e: DiscreteExperiment, maxHammingDistance: Int
  ): UndirectedStateGraph = {
    val stateToMeasurements = e.measurements.groupBy(_.values)

    val V = stateToMeasurements map {
      case (state, ms) => DiscreteStateGraphNode(state, ms)
    }

    var g = new UndirectedStateGraph(V.toSet, Set.empty, e.names)

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

  class UndirectedStateGraph(
    val V: Set[DiscreteStateGraphNode],
    val E: Set[DiscreteStateGraphEdge],
    val names: Seq[String]
  ) extends UndirectedGraph {
    type Vertex = DiscreteStateGraphNode
    type Edge = DiscreteStateGraphEdge

    def addVertex(v: DiscreteStateGraphNode): UndirectedStateGraph = {
      new UndirectedStateGraph(V + v, E, names)
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
    }

    def edgeLabels(e: DiscreteStateGraphEdge): Seq[String] = {
      DiscreteStateAnalysis.nonIdenticalNames(names, e.n1.state, e.n2.state)
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

  class DirectedStateGraph(
    override val V: Set[DiscreteStateGraphNode],
    override val E: Set[DiscreteStateGraphEdge],
    val edgeDirections: mutable.MultiMap[DiscreteStateGraphEdge, EdgeDirection],
    override val names: Seq[String]
  ) extends UndirectedStateGraph(V, E, names) {
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

  case class DiscreteStateGraphNode(
    state: Seq[Int],
    measurements: Seq[DiscreteMeasurement]
  )
    extends Ordered[DiscreteStateGraphNode] {
    override def compare(o: DiscreteStateGraphNode): Int = {
      assert(this.state.size == o.state.size)

      import scala.math.Ordering.Implicits._
      if (this.state < o.state) {
        -1
      } else if (this.state == o.state) {
        0
      } else {
        1
      }
    }
  }

  case class DiscreteStateGraphEdge(
    n1: DiscreteStateGraphNode,
    n2: DiscreteStateGraphNode
  )
}
