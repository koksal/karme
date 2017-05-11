package karme.transformations

import karme.CellTrajectories.CellTrajectory
import karme.Experiments.Experiment
import karme.Reporter
import karme.analysis.DiscreteStateAnalysis
import karme.graphs.StateGraphs
import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.graphs.StateGraphs.StateGraphVertex
import karme.util.MathUtil
import karme.util.TimingUtil
import karme.visualization.StateGraphPlotter

class IncrementalStateGraphBuilder(
  exp: Experiment[Boolean],
  trajectories: Seq[CellTrajectory],
  reporter: Reporter
) {

  val MAX_HAMMING_DISTANCE = 3

  val V = StateGraphs.nodesFromExperiment(exp)

  lazy val nodePartialOrdering = new NodePartialOrderByPseudotimeRankSum(
    V.toSeq, trajectories).partialOrdering

  lazy val initialNodes: Set[StateGraphVertex] = {
    TimingUtil.time("finding initial nodes using PO") {
      V filter { candidateV =>
        !V.exists(otherV => nodePartialOrdering.lt(otherV, candidateV))
      }
    }
  }

  lazy val buildGraph: DirectedBooleanStateGraph = {
    var graph = new DirectedBooleanStateGraph()
    for (n <- initialNodes) {
      graph = graph.addVertex(n)
    }

    var keepSearching = true
    while (keepSearching) {
      chooseMinimalHammingNeighbor(graph.V) match {
        case Some((source, neighbor)) => {
          println("found new neighbor to add...")
          graph = graph.addEdge(source, neighbor)
        }
        case None => {
          println("No new neighbors")
          keepSearching = false
        }
      }
    }

    graph
  }

  private def chooseMinimalHammingNeighbor(
    reachableNodes: Set[StateGraphVertex]
  ): Option[(StateGraphVertex, StateGraphVertex)] = {
    val distancesToNeighbors = hammingDistancesToTargets(
      reachableNodes, V -- reachableNodes)

    val validNeighbors = distancesToNeighbors filter {
      case (source, target, distance) => {
        nodePartialOrdering.lt(source, target) &&
          distance < MAX_HAMMING_DISTANCE
      }
    }

    if (validNeighbors.isEmpty) {
      None
    } else {
      val minDistanceTriplet = validNeighbors.minBy(_._3)
      Some((minDistanceTriplet._1, minDistanceTriplet._2))
    }
  }

  private def hammingDistancesToTargets(
    sources: Set[StateGraphVertex],
    targets: Set[StateGraphVertex]
  ): Set[(StateGraphVertex, StateGraphVertex, Int)] = {
    TimingUtil.time("Hamming distances to targets") {
      MathUtil.cartesianProduct(List(sources, targets)) map {
        case List(source, target) => {
          (source, target,
            DiscreteStateAnalysis.hammingDistance(source.state, target.state))
        }
      }
    }
  }

}
