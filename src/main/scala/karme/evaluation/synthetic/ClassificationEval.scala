package karme.evaluation.synthetic

object ClassificationEval {

  private val tprHeader = "TPR"
  private val fdrHeader = "FDR"
  val headers = List(tprHeader, fdrHeader)

  def evaluate[A](
    predicted: Set[A],
    positive: Set[A],
    negative: Set[A]
  ): Map[String, Double] = {
    val tp = predicted.intersect(positive)
    val fp = predicted.intersect(negative)

    val tpr = tp.size.toDouble / positive.size.toDouble
    val fdr = fp.size.toDouble / predicted.size.toDouble

    Map(
      tprHeader -> tpr,
      fdrHeader -> fdr
    )
  }
}
