package karme.transformations

import karme.Clustering
import karme.Experiments.Experiment
import karme.evaluation.RankSumTest
import karme.graphs.Graphs.UnlabeledEdge
import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.graphs.StateGraphs.StateGraphVertex
import karme.graphs.StateGraphs.UndirectedStateGraphOps
import karme.util.MathUtil

class ClusteringRefiner(
  clusterLevelGraph: DirectedBooleanStateGraph,
  geneLevelExp: Experiment[Double],
  clustering: Clustering
) {

  val P_VALUE_THRESHOLD = 0.01

  val ALL_GENES = clustering.allMembers

  def refineClusteringPerEdge():
      Map[UnlabeledEdge[StateGraphVertex], Clustering] = {
    val edgeToRefinedClustering = for (e <- clusterLevelGraph.E) yield {
      e -> Clustering(refineClusteringForEdgeLabels(e))
    }

    edgeToRefinedClustering.toMap
  }

  def refineClusteringForEdgeLabels(
    e: UnlabeledEdge[StateGraphVertex]
  ): Map[String, Set[String]] = {
    var refinedClustering = Map[String, Set[String]]()

    println(s"Edge between ${e.v1.id} and ${e.v2.id}:")

    for (label <- UndirectedStateGraphOps.edgeLabels(e)) {
      val upregulated = clusterIsUpregulated(e, label)

      val clusterMembers = clustering.clusterToMember(label)

      val agreeingGenes = clusterMembers filter { g =>
        geneAgreesWithSwitch(e, g, upregulated)
      }

      println(s"Label: $label")
      println(s"Cluster size: ${clusterMembers.size}")
      println(s"Agreeing genes in cluster: ${agreeingGenes.size}")

      refinedClustering += label -> agreeingGenes
    }

    refinedClustering
  }

  def clusterIsUpregulated(
    e: UnlabeledEdge[StateGraphVertex], label: String
  ): Boolean = {
    val leftVal = e.v1.state.value(label)
    val rightVal = e.v2.state.value(label)

    assert(leftVal != rightVal)

    !leftVal
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

    val res = new RankSumTest(greater, smaller).run()
    res.pValue <= P_VALUE_THRESHOLD
  }

  def analyzeRefinedClusters(
    edgeToClustering: Map[UnlabeledEdge[StateGraphVertex], Clustering]
  ): Unit = {
    // how many clusters does each gene belong to?

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
