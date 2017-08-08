package karme.transformations

import karme.Clustering
import karme.graphs.Graphs
import karme.graphs.Graphs.UnlabeledEdge
import karme.graphs.StateGraphs.UndirectedStateGraphOps
import karme.graphs.StateGraphs.{DirectedBooleanStateGraph, StateGraphVertex}

case class EdgePrecedence(source: String, target: String, distance: Int)

class EdgePrecedenceProducer(
  g: DirectedBooleanStateGraph,
  edgeToRefinedClustering: Map[UnlabeledEdge[StateGraphVertex], Clustering]
) {

  def computePrecedence: Seq[EdgePrecedence] = {
    g.V.toSeq.flatMap(v => computePrecedenceFromNode(v))
  }

  def computePrecedenceFromNode(v: StateGraphVertex): Seq[EdgePrecedence] = {
    val shortestPaths = g.shortestPaths(v)
    val nonTrivialPaths = shortestPaths.filter(_.size >= 3)

    nonTrivialPaths.toSeq flatMap computeExpandedPrecedences
  }

  def computeExpandedPrecedences(
    p: Seq[StateGraphVertex]
  ): Seq[EdgePrecedence] = {
    require(p.size >= 3)

    val distance = p.size - 2
    val firstEdge = Graphs.lexicographicEdge(p(0), p(1))
    val lastEdge = Graphs.lexicographicEdge(p(p.size - 2), p(p.size - 1))

    val pairSeqs = for {
      l1 <- UndirectedStateGraphOps.edgeLabels(firstEdge)
      l2 <- UndirectedStateGraphOps.edgeLabels(lastEdge)
    } yield {
      // get refined clustering for both edges
      val sourceGenes = edgeToRefinedClustering(firstEdge).clusterToMembers(l1)
      val targetGenes = edgeToRefinedClustering(lastEdge).clusterToMembers(l2)

      // compute product given refined members
      computeNonSelfProduct(sourceGenes, targetGenes, distance)
    }

    pairSeqs.flatten
  }

  def computeNonSelfProduct(
    sources: Set[String], targets: Set[String], distance: Int
  ): Seq[EdgePrecedence] = {
    for {
      source <- sources.toSeq
      target <- targets.toSeq
      if source != target
    } yield {
      EdgePrecedence(source, target, distance)
    }
  }

}
