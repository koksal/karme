package karme.transformations.clustering

import karme.{Clustering, PredictionLibrary, ReferencePrediction}

object ReferenceClustering {

  def clusterTargetsByCommonResponse(library: PredictionLibrary): Clustering = {
    val sources = library.predictions.map(_.source).distinct
    val targets = library.predictions.map(_.target).distinct
    val targetToPredictions = library.predictions.groupBy(_.target)

    val foldChangeMatrix = for (target <- targets) yield {
      val predsToTarget = targetToPredictions(target)
      sources map (s => getFoldChange(s, predsToTarget))
    }

    // TODO count unique clusters.
    val uniqueRows = foldChangeMatrix.distinct
    println(s"All rows: ${foldChangeMatrix.size}")
    println(s"Distinct rows: ${uniqueRows.size}")

    // val nbClust = new NbClustInterface()
    // val clusterIndices = nbClust.cluster(foldChangeMatrix, 2, 20)

    // GeneClustering.makeClustering(targets, clusterIndices)
    ???
  }

  def getFoldChange(source: String, preds: Seq[ReferencePrediction]): Int = {
    preds.find(_.source == source) match {
      case Some(p) => p.weight.signum
      case None => 0
    }
  }

}
