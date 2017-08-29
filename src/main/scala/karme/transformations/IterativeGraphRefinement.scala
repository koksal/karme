package karme.transformations

import karme.Clustering
import karme.Experiments.Experiment
import karme.Reporter
import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.graphs.StateGraphs.StateGraphVertex
import karme.transformations.ExpressionDerivation.Unchanged
import karme.transformations.clustering.DerivativeClustering
import karme.util.FileUtil
import karme.visualization.CurvePlot
import karme.visualization.StateGraphPlotter

class IterativeGraphRefinement(
  inputTransformer: InputTransformer
)(implicit reporter: Reporter) {

  def refineGraphIteratively(
    initialClusteredGraph: DirectedBooleanStateGraph,
    initialSources: Set[StateGraphVertex],
    initialClustering: Clustering,
    geneLevelExp: Experiment[Double]
  ) = {
    var clusteredGraph = initialClusteredGraph
    var sources = initialSources
    var clustering = initialClustering
    var prevClustering = clustering

    var i = 0
    do {
      new CurvePlot().plotClusterCurves(geneLevelExp,
        inputTransformer.trajectories, clustering.clusterToMembers,
        s"iterative-clustering-$i")
      new StateGraphPlotter(reporter).plotDirectedGraph(
        clusteredGraph,
        s"state-graph-$i",
        cellClustering = inputTransformer.annotationContext.cellClustering,
        nodeHighlightGroups = List(sources.map(_.state))
      )

      println("Refining clustering and graph.")
      i += 1

      prevClustering = clustering

      val derivatives = new ClusteringRefiner(
        clusteredGraph, geneLevelExp, clustering,
        DistributionComparisonTest.fromOptions(
          inputTransformer.opts.distributionComparisonMethod),
        inputTransformer.opts.clusteringOpts.clusterRefinementPValue)
        .deriveGenesOnEdges()

      FileUtil.writeToFile(reporter.file(s"derivatives-$i.txt"),
        derivatives.mkString("\n"))

      val nonConstantDerivatives = derivatives filter {
        case (name, ds) => ds.exists(d => d != Unchanged)
      }
      println(s"# Non constant derivatives: ${nonConstantDerivatives.size}")

      clustering = new DerivativeClustering(inputTransformer.clusteringModule)
        .clusterGenes(nonConstantDerivatives)

      FileUtil.writeToFile(reporter.file(s"clustering-$i.txt"),
        clustering.clusterToMembers.mkString("\n"))

      val avgExp = inputTransformer.clusteringModule
        .experimentFromClusterAverages(geneLevelExp,
          clustering.clusterToMembers)
      val (newGraph, newSources) = inputTransformer.makeGraphAndSources(avgExp)
      clusteredGraph = newGraph
      sources = newSources

    } while (prevClustering != clustering)

    (clusteredGraph, sources, clustering)
  }

}
