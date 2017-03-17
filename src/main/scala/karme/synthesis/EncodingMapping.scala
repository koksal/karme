package karme.synthesis

import karme.synthesis.Trees.Expr
import karme.synthesis.Trees.IntLiteral

class EncodingMapping(variableNames: Seq[String]) {
  sealed trait EncodingMappingValue
  case object IgnoreEncodingValue extends EncodingMappingValue
  case object NotEncodingValue extends EncodingMappingValue
  case object AndEncodingValue extends EncodingMappingValue
  case object OrEncodingValue extends EncodingMappingValue

  // TODO It is important that the "not" value is smaller than other
  // non-ignore values to make symmetry breaking work with functions of the
  // form !f_1 && f_2. Can we eliminate this implicit dependence?
  private val nonVariableEncodingValues = List(IgnoreEncodingValue,
    NotEncodingValue, AndEncodingValue, OrEncodingValue)

  private def encodingValueConstant(emv: EncodingMappingValue): Int = {
    assert(nonVariableEncodingValues.contains(emv))
    nonVariableEncodingValues.indexOf(emv)
  }

  val IGNORE_NODE = IntLiteral(encodingValueConstant(IgnoreEncodingValue))
  val NOT_NODE = IntLiteral(encodingValueConstant(NotEncodingValue))
  val AND_NODE = IntLiteral(encodingValueConstant(AndEncodingValue))
  val OR_NODE = IntLiteral(encodingValueConstant(OrEncodingValue))

  def VAR_NODE(name: String): Expr = {
    IntLiteral(nonVariableEncodingValues.size + variableNames.indexOf(name))
  }

  val VAR_NODE_RANGE: Seq[Expr] = variableNames map VAR_NODE

  def VAR_NAME(modelValue: Int): String = {
    variableNames(modelValue - nonVariableEncodingValues.size)
  }
}

