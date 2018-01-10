package karme.printing

import karme.synthesis.FunctionTrees._

object FunExprPrettyPrinter {

  def printPlain(fe: FunExpr): String = {
    plainStr(fe, precedence(fe))
  }

  def printLaTeX(fe: FunExpr): String = {
    LatexPrinting.latexMath(latexStr(fe, precedence(fe)))
  }

  private def plainStr(fe: FunExpr, outerPrecedence: Int): String = {
    val currentPrecedence = precedence(fe)
    val result = fe match {
      case FunConst(v) => v.toString
      case FunVar(id)   => id
      case FunAnd(l, r) => s"${plainStr(l, currentPrecedence)} && " +
        s"${plainStr(r, currentPrecedence)}"
      case FunOr(l, r)  => s"${plainStr(l, currentPrecedence)} || " +
        s"${plainStr(r, currentPrecedence)}"
      case FunNot(e)    => s"!${plainStr(e, currentPrecedence)}"
    }
    if (currentPrecedence > outerPrecedence) {
      s"(${result})"
    } else {
      result
    }
  }

  private def latexStr(fe: FunExpr, outerPrecedence: Int): String = {
    val currentPrecedence = precedence(fe)
    val result = fe match {
      case FunConst(v) => LatexPrinting.latexifyBool(v)
      case FunVar(id) => LatexPrinting.latexifyId(id)
      case FunAnd(l, r) => s"${latexStr(l, currentPrecedence)} \\wedge " +
        s"${latexStr(r, currentPrecedence)}"
      case FunOr(l, r)  => s"${latexStr(l, currentPrecedence)} \\vee " +
        s"${latexStr(r, currentPrecedence)}"
      case FunNot(e)    => s"\\neg ${latexStr(e, currentPrecedence)}"
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
