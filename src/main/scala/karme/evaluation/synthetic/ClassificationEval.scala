package karme.evaluation.synthetic

object ClassificationEval {

  private val tprHeader = "TPR"
  private val fprHeader = "FPR"
  val headers = List(tprHeader, fprHeader)

  def evaluate[A](
    predicted: Set[A],
    positive: Set[A],
    negative: Set[A]
  ): Map[String, Any] = {
    val tp = predicted.intersect(positive)
    val fp = predicted.intersect(negative)

    val tpr = tp.size.toDouble / positive.size.toDouble
    val fpr = fp.size.toDouble / negative.size.toDouble

    Map(
      tprHeader -> tpr,
      fprHeader -> fpr
    )
  }
}
