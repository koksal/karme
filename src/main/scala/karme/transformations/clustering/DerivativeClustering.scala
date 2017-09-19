package karme.transformations.clustering

import karme.Clustering
import karme.transformations.ExpressionDerivation.{Downregulated, ExpressionDerivative, Unchanged, Upregulated}

class DerivativeClustering(geneClustering: GeneClustering) {

  def clusterGenes(
    geneToDerivatives: Map[String, Seq[ExpressionDerivative]]
  ): Clustering = {
    geneClustering.computeBestClustering(
      sortedGenes(geneToDerivatives),
      valueMatrix(geneToDerivatives)
    )
  }

  private def sortedGenes(
    geneToDerivatives: Map[String, Seq[ExpressionDerivative]]
  ): Seq[String] = {
    geneToDerivatives.keySet.toList.sorted
  }

  private def valueMatrix(
    geneToDerivatives: Map[String, Seq[ExpressionDerivative]]
  ): Seq[Seq[Double]] = {
    for (gene <- sortedGenes(geneToDerivatives)) yield {
      geneToDerivatives(gene) map deriv2double
    }
  }

  private def deriv2double(d: ExpressionDerivative): Double = d match {
    case Upregulated => 1
    case Downregulated => -1
    case Unchanged => 0
  }

}
