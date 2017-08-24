package karme.transformations.clustering

import karme.Clustering
import karme.evaluation.enrichr.PredictionLibrary
import karme.evaluation.enrichr.ReferencePrediction

class ReferenceClustering {

  val nbClust = new NbClustInterface()

  def clusterTargetsByCommonResponse(library: PredictionLibrary): Clustering = {
    val (sources, targets) = library.ioPairs.toSeq.unzip
    val targetToPredictions = library.predictions.groupBy(_.target)

    val foldChangeMatrix = for (target <- targets) yield {
      val predsToTarget = targetToPredictions(target)
      sources map (s => getFoldChange(s, predsToTarget))
    }

    val clusterIndices = nbClust.cluster(foldChangeMatrix, 2, 30)

    GeneClustering.makeClustering(targets, clusterIndices)
  }

  def getFoldChange(source: String, preds: Seq[ReferencePrediction]): Double = {
    preds.find(_.term == source) match {
      case Some(p) => p.weight
      case None => 0.0
    }
  }

}
