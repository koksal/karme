package karme.graphs

import karme.CellTrajectories.CellTrajectory
import karme.Experiments
import karme.Experiments.{DiscreteExperiment, DiscreteMeasurement, TriValuedExperiment}
import karme.discretization.Discretization
import karme.graphs.Graphs._
import karme.transformations.DiscreteStateAnalysis
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.util.MathUtil

import scala.collection.mutable

object StateGraphs {

  type UndirectedBooleanStateGraph = UnlabeledGraph[StateGraphVertex]
  type DirectedBooleanStateGraph = UnlabeledDiGraph[StateGraphVertex]


  def fromDiscreteExperiment(
    discreteExperiment: DiscreteExperiment,
    maxHammingDistance: Int
  ): UndirectedBooleanStateGraph = {
    val stateToMeasurements = discreteExperiment.measurements.groupBy(_.values)

    val V = stateToMeasurements map {
      case (state, ms) =>
        val booleanValues = state map { v => v == Discretization.HIGH_VALUE }
        val booleanState = ConcreteBooleanState(
          discreteExperiment.names.zip(booleanValues).toMap)
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

  def fromTriValuedExperiment(
    triValuedExperiment: TriValuedExperiment,
    maxHammingDistance: Int
  ): UndirectedBooleanStateGraph = {
    val stateToMeasurements = triValuedExperiment.measurements.groupBy(_.values)

    // compute vertex groups, each group corresponds to one tri-valued state
    // measurements are duplicated across Boolean states mapping to the same
    // tri-valued state.
    val vertexGroups = stateToMeasurements map {
      case (state, ms) =>
        // compute Set of booleans for each
        val booleanSets = state map Experiments.threeValuedToBooleanSet
        // take cartesian product of set
        val cartesianProduct = MathUtil.cartesianProduct(booleanSets.toList)

        val vertexGroup = cartesianProduct map { prod =>
          val concreteBooleanState = ConcreteBooleanState(
            triValuedExperiment.names.zip(prod).toMap)
          // TODO cleanup
          val intValues = prod map (v =>
            if (v) Discretization.HIGH_VALUE else Discretization.LOW_VALUE)
          val newMs = ms.map(m => m.copy(values = intValues))
          StateGraphVertex(concreteBooleanState, newMs)
        }
        vertexGroup
    }

    // create graph with all vertices
    var g = new UndirectedBooleanStateGraph(V = vertexGroups.flatten.toSet)

    // add edges between every pair of nodes except within the set
    val vertexGroupSeq = vertexGroups.toIndexedSeq
    for {
      i <- 0 until vertexGroupSeq.size
      j <- (i + 1) until vertexGroupSeq.size
    } {
      val vertexGroup1 = vertexGroupSeq(i)
      val vertexGroup2 = vertexGroupSeq(j)

      for {
        v1 <- vertexGroup1
        v2 <- vertexGroup2
      } {
        val dist = DiscreteStateAnalysis.hammingDistance(v1.state, v2.state)
        if (dist <= maxHammingDistance) {
          g = g.addEdge(v1, v2)
        }
      }
    }

    g
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

      // check that inferred directions are not contradictory & merge directions
      for (edge <- g.E) {
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
      new DirectedBooleanStateGraph(g.V, directions.keySet.toSet, directions)
    }

    private def trajectoryDirections(
      g: UndirectedBooleanStateGraph,
      trajectory: CellTrajectory
    ): Map[UnlabeledEdge[StateGraphVertex], EdgeDirection] = {
      var res = Map[UnlabeledEdge[StateGraphVertex], EdgeDirection]()

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
            val dir = if (pt1 < pt2) Forward else Backward
            res += e -> dir
          }
          case _ =>
        }
      }

      res
    }

  }

  private def avgNodePseudotime(
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

}
