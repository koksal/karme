package karme.transformations

object ExpressionDerivation {

  sealed trait ExpressionDerivative
  case object Upregulated extends ExpressionDerivative
  case object Downregulated extends ExpressionDerivative
  case object Unchanged extends ExpressionDerivative

}
