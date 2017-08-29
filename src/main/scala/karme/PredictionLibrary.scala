package karme

case class PredictionLibrary(
  id: String,
  predictions: Seq[ReferencePrediction]
) {

  def names: Set[String] = {
    (predictions flatMap {
      case ReferencePrediction(source, target, _) => Set(source, target)
    }).toSet
  }

  def ioPairs: Set[(String, String)] = {
    predictions.map{
      case ReferencePrediction(source, target, _) => (source, target)
    }.toSet
  }

  def sources: Set[String] = predictions.map(_.source).toSet
  def targets: Set[String] = predictions.map(_.target).toSet

}

case class ReferencePrediction(
  source: String, target: String, weight: Double
)