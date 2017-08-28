package karme.evaluation

object PredictionTypes {

  sealed trait PredictionType
  case object FunIOPairsPrediction extends PredictionType
  case object PrecedencePairsPrediction extends PredictionType

}
