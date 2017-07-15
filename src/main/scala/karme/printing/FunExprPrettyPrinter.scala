package karme.printing

import karme.synthesis.FunctionTrees.FunAnd
import karme.synthesis.FunctionTrees.FunConst
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.FunctionTrees.FunNot
import karme.synthesis.FunctionTrees.FunOr
import karme.synthesis.FunctionTrees.FunVar

object FunExprPrettyPrinter {

  def apply(fe: FunExpr): String = {
    prettyString(fe, precedence(fe))
  }

  private def prettyString(fe: FunExpr, outerPrecedence: Int): String = {
    val currentPrecedence = precedence(fe)
    val result = fe match {
      case FunConst(v) => v.toString
      case FunVar(id)   => id
      case FunAnd(l, r) => s"${prettyString(l, currentPrecedence)} && " +
        s"${prettyString(r, currentPrecedence)}"
      case FunOr(l, r)  => s"${prettyString(l, currentPrecedence)} || " +
        s"${prettyString(r, currentPrecedence)}"
      case FunNot(e)    => s"!${prettyString(e, currentPrecedence)}"
    }
    if (currentPrecedence > outerPrecedence) {
      s"(${result})"
    } else {
      result
    }
  }

  private def precedence(fe: FunExpr): Int = fe match {
    case FunConst(_) => 0
    case FunVar(_) => 0
    case FunAnd(_, _) => 1
    case FunOr(_, _) => 2
    case FunNot(_) => 0
  }

}
