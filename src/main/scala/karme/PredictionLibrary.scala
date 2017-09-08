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

object PredictionLibrary {

  def checkOppositePredictions(l: PredictionLibrary): Unit = {
    val sources = l.predictions.map(_.source).toSet
    println(s"Number of sources: ${sources.size}")

    val pairToPreds = l.predictions.groupBy(p => (p.source, p.target))

    var oppositeFound = false
    for ((src, tgt) <- pairToPreds.keySet; if src != tgt) {
      pairToPreds.get((tgt, src)) match {
        case Some(oppositePredictions) => {
          println("Opposite edges found in library:")
          println(pairToPreds((src, tgt)).mkString("\n"))
          println(pairToPreds((tgt, src)).mkString("\n"))
          oppositeFound = true
        }
        case None =>
      }
    }
    if (!oppositeFound) {
      println(s"No opposite edges found in ${l.id}")
    }
  }

  def aggregate(ls: Seq[PredictionLibrary]): PredictionLibrary = {
    val allPredictions = ls.flatMap(_.predictions)
    val ioPairToPredictions = allPredictions.groupBy(p => (p.source, p.target))

    val aggregatedPredictions = ioPairToPredictions map {
      case (pair, predictions) => {
        aggregatePredictions(predictions)
      }
    }

    PredictionLibrary(makeAggregatedID(ls), aggregatedPredictions.toSeq)
  }

  private def aggregatePredictions(
    ps: Seq[ReferencePrediction]
  ): ReferencePrediction = {
    val signumSet = ps.map(p => p.weight.signum).toSet
    if (signumSet.contains(-1) && signumSet.contains(1)) {
      println("Conflicting reference weights:")
      println(ps.mkString("\n"))
    }

    val weightWithMaxMagnitude = ps.map(_.weight).maxBy(w => math.abs(w))

    ps.head.copy(weight = weightWithMaxMagnitude)
  }

  private def makeAggregatedID(ls: Seq[PredictionLibrary]): String = {
    ls.map(_.id).mkString("-")
  }

}

case class ReferencePrediction(
  source: String, target: String, weight: Double
)
