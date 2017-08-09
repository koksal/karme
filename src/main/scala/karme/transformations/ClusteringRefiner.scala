package karme.transformations

import karme.Clustering
import karme.Experiments.Experiment
import karme.graphs.Graphs.UnlabeledEdge
import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.graphs.StateGraphs.StateGraphVertex
import karme.graphs.StateGraphs.UndirectedStateGraphOps
import karme.transformations.ExpressionDerivation.Downregulated
import karme.transformations.ExpressionDerivation.ExpressionDerivative
import karme.transformations.ExpressionDerivation.Unchanged
import karme.transformations.ExpressionDerivation.Upregulated
import karme.util.MathUtil

class ClusteringRefiner(
  clusterLevelGraph: DirectedBooleanStateGraph,
  geneLevelExp: Experiment[Double],
  clustering: Clustering,
  distributionComparisonTest: DistributionComparisonTest,
  pValueThreshold: Double
) {
  val ALL_GENES = clustering.allMembers.toSeq
  val ALL_EDGES = clusterLevelGraph.E.toSeq

  def refineClusteringPerEdge():
  Map[UnlabeledEdge[StateGraphVertex], Clustering] = {
    val edgeToRefinedClustering = for (e <- clusterLevelGraph.E) yield {
      e -> Clustering(refineClusteringForEdgeLabels(e))
    }

    edgeToRefinedClustering.toMap
  }

  private def refineClusteringForEdgeLabels(
    e: UnlabeledEdge[StateGraphVertex]
  ): Map[String, Set[String]] = {
    var refinedClustering = Map[String, Set[String]]()

    println(s"Edge between ${e.v1.id} and ${e.v2.id}:")

    for (label <- UndirectedStateGraphOps.edgeLabels(e)) {
      val upregulated = clusterIsUpregulated(e, label)

      val clusterMembers = clustering.clusterToMembers(label)

      val agreeingGenes = clusterMembers filter { g =>
        geneAgreesWithSwitch(e, g, upregulated)
      }

      println(s"Label: $label")
      println(s"Cluster size: ${clusterMembers.size}")
      println(s"Agreeing genes in cluster: ${agreeingGenes.size}")
      println(agreeingGenes.toList.sorted.mkString(", "))

      refinedClustering += label -> agreeingGenes
    }

    refinedClustering
  }

  private def clusterIsUpregulated(
    e: UnlabeledEdge[StateGraphVertex], label: String
  ): Boolean = {
    val leftVal = e.v1.state.value(label)
    val rightVal = e.v2.state.value(label)

    assert(leftVal != rightVal)

    !leftVal
  }

  def deriveGenesOnEdges(): Map[String, Seq[ExpressionDerivative]] = {
    var geneToDerivatives = Map[String, Seq[ExpressionDerivative]]()

    for (gene <- ALL_GENES) {
      val derivatives = for (e <- ALL_EDGES) yield {
        deriveGeneOnEdge(e, gene)
      }
      geneToDerivatives += gene -> derivatives
    }

    geneToDerivatives
  }

  private def deriveGeneOnEdge(
    edge: UnlabeledEdge[StateGraphVertex], gene: String
  ): ExpressionDerivative = {
    if (geneAgreesWithSwitch(edge, gene, true)) {
      Upregulated
    } else if (geneAgreesWithSwitch(edge, gene, false)) {
      Downregulated
    } else {
      Unchanged
    }
  }

  def geneAgreesWithSwitch(
    e: UnlabeledEdge[StateGraphVertex],
    name: String,
    switchIsUpregulation: Boolean
  ): Boolean = {
    val leftMeasurementIds = e.v1.measurements.map(_.id)
    val rightMeasurementIds = e.v2.measurements.map(_.id)

    val leftGeneValues = leftMeasurementIds map { id =>
      geneLevelExp.measurementFromId(id).state.value(name)
    }

    val rightGeneValues = rightMeasurementIds map { id =>
      geneLevelExp.measurementFromId(id).state.value(name)
    }

    val (greater, smaller) = if (switchIsUpregulation) {
      (rightGeneValues, leftGeneValues)
    } else {
      (leftGeneValues, rightGeneValues)
    }

    distributionComparisonTest.testPValue(greater, smaller) <= pValueThreshold
  }

  def analyzeRefinedClusters(
    edgeToClustering: Map[UnlabeledEdge[StateGraphVertex], Clustering]
  ): Unit = {
    val memberClusterPairs =
      edgeToClustering.toList.map(_._2.memberToCluster.toList).flatten

    val memberToClusterSet = memberClusterPairs.groupBy(_._1) map {
      case (member, pairs) => member -> (pairs.map(_._2).toSet)
    }

    val allNbClusters = memberToClusterSet.map(_._2.size)
    val medianNbClusters = MathUtil.median(allNbClusters)

    println(s"Median # clusters a gene belongs to: $medianNbClusters")
    println(memberToClusterSet.mkString("\n"))
  }
}
