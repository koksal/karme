package karme.synthesis

import karme.synthesis.Trees.Expr
import karme.synthesis.Trees.IntLiteral

class EncodingMapping(variableNames: Seq[String]) {
  sealed trait EncodingMappingValue
  case object AndEncodingValue extends EncodingMappingValue
  case object OrEncodingValue extends EncodingMappingValue
  case object NotEncodingValue extends EncodingMappingValue
  case object IgnoreEncodingValue extends EncodingMappingValue

  private val nonVariableEncodingValues = List(AndEncodingValue,
    OrEncodingValue, NotEncodingValue, IgnoreEncodingValue)

  private def encodingValueConstant(emv: EncodingMappingValue): Int = {
    assert(nonVariableEncodingValues.contains(emv))
    nonVariableEncodingValues.indexOf(emv)
  }

  val AND_NODE = IntLiteral(encodingValueConstant(AndEncodingValue))
  val OR_NODE = IntLiteral(encodingValueConstant(OrEncodingValue))
  val NOT_NODE = IntLiteral(encodingValueConstant(NotEncodingValue))
  val IGNORE_NODE = IntLiteral(encodingValueConstant(IgnoreEncodingValue))

  def VAR_NODE(name: String): Expr = {
    IntLiteral(nonVariableEncodingValues.size + variableNames.indexOf(name))
  }

  val VAR_NODE_RANGE: Seq[Expr] = variableNames map VAR_NODE

  def VAR_NAME(modelValue: Int): String = {
    variableNames(modelValue - nonVariableEncodingValues.size)
  }
}

