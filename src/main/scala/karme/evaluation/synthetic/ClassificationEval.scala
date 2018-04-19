package karme.evaluation.synthetic

object ClassificationEval {

  private val tprHeader = "TPR"
  private val fdrHeader = "FDR"
  private val precisionHeader = "Precision"
  private val recallHeader = "Recall"
  private val f1ScoreHeader = "F1 Score"

  val headers = List(tprHeader, fdrHeader, precisionHeader, recallHeader,
    f1ScoreHeader)

  def evaluate[A](
    predicted: Set[A],
    positive: Set[A],
    negative: Set[A]
  ): Map[String, Double] = {
    val tp = predicted.intersect(positive)
    val fp = predicted.intersect(negative)

    val tpr = tp.size.toDouble / positive.size.toDouble
    val fdr = fp.size.toDouble / predicted.size.toDouble

    val precision = tp.size.toDouble / predicted.size.toDouble
    val recall = tp.size.toDouble / positive.size.toDouble
    val f1Score = getF1Score(precision, recall)

    Map(
      tprHeader -> tpr,
      fdrHeader -> fdr,
      precisionHeader -> precision,
      recallHeader -> recall,
      f1ScoreHeader -> f1Score
    )
  }

  def getF1Score(precision: Double, recall: Double): Double = {
    val sum = precision + recall
    val product = precision * recall

    2 * product / sum
  }
}
