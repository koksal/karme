package karme.synthesis

import karme.synthesis.Transitions.ConcreteBooleanState
import karme.synthesis.Trees._

object FunctionTrees {

  sealed trait FunExpr
  case class FunVar(id: String) extends FunExpr
  case class FunAnd(l: FunExpr, r: FunExpr) extends FunExpr
  case class FunOr(l: FunExpr, r: FunExpr) extends FunExpr
  case class FunNot(e: FunExpr) extends FunExpr

  def eval(fe: FunExpr, in: ConcreteBooleanState): Boolean = fe match {
    case FunVar(id) => in(id)
    case FunAnd(l, r) => eval(l, in) && eval(r, in)
    case FunOr(l, r) => eval(l, in) || eval(r, in)
    case FunNot(e) => !eval(e, in)
  }

  def pprint(fe: FunExpr): String = fe match {
    case FunVar(id)   => id
    case FunAnd(l, r) => s"(${pprint(l)}) && (${pprint(r)})"
    case FunOr(l, r)  => s"(${pprint(l)}) || (${pprint(r)})"
    case FunNot(e)    => s"!(${pprint(e)})"
  }

  class EncodingMapping(prots: Seq[String]) {
    // constant values
    val AND       = 0
    val OR        = 1
    val NOT       = 2
    val IGNORE    = 3
    val VARSTART  = 4

    // expressions
    val AND_NODE                = IntLiteral(AND)
    val OR_NODE                 = IntLiteral(OR)
    val NOT_NODE                = IntLiteral(NOT)
    val IGNORE_NODE             = IntLiteral(IGNORE)
    def VAR_NODE(name: String)  = IntLiteral(prots.indexOf(name) + VARSTART)
    val VAR_NODE_RANGE          = prots map VAR_NODE

    def VAR_NAME(modelValue: Int) = prots(modelValue - this.VARSTART)
  }

  abstract class SymFunExpr {
    val possibleVars: Set[String]
    val prots: List[String] = possibleVars.toList.sorted
    val encodingMapping = new EncodingMapping(prots)

    def nodeValue: Variable
    def children: List[SymFunExpr]
    def descendants: List[SymFunExpr]

    def consistency(): Expr = {
      val allNodes = this :: this.descendants
      // all variables are distinct
      val distinctVars = for (
        n1 <- allNodes; n2 <- allNodes; if n1 != n2
      ) yield {
        Implies(
          And(n1.isVAR, n2.isVAR),
          Not(Equals(n1.nodeValue, n2.nodeValue))
        )
      }
      And(
        Not(this.isIGNORE),
        And(distinctVars: _*),
        this.recursiveConsistency()
      )
    }

    def recursiveConsistency(): Expr = {
      And(
        this.nodeConsistency(),
        And(
          this.children.map(_.recursiveConsistency()): _*
        )
      )
    }

    def nodeConsistency(): Expr

    def isAND: Expr = Equals(this.nodeValue, encodingMapping.AND_NODE)
    def isOR: Expr = Equals(this.nodeValue, encodingMapping.OR_NODE)
    def isNOT: Expr = Equals(this.nodeValue, encodingMapping.NOT_NODE)
    def isIGNORE: Expr = Equals(this.nodeValue, encodingMapping.IGNORE_NODE)
    def isPROT(name: String) =
      Equals(this.nodeValue, encodingMapping.VAR_NODE(name))

    def isVAR: Expr = {
      val v = this.nodeValue
      val disj = encodingMapping.VAR_NODE_RANGE map { vn =>
        Equals(v, vn)
      }
      Or(disj.toList: _*)
    }

    def isGate: Expr = {
      Or(this.isAND, this.isOR, this.isNOT)
    }
  }

  object SymFunTree {
    def unapply(t: SymFunTree): Option[(SymFunExpr, Variable, SymFunExpr)] = {
      Some((t.l, t.v, t.r))
    }
  }

  class SymFunTree(
    val l: SymFunExpr,
    val v: Variable,
    val r: SymFunExpr,
    val possibleVars: Set[String]
  ) extends SymFunExpr {
    def nodeValue: Variable = v
    def children = List(l, r)
    def descendants = l.descendants ::: r.descendants ::: List(l, r)

    def nodeConsistency(): Expr = {
      val andCase = And(
        this.isAND,
        Not(l.isIGNORE),
        Not(r.isIGNORE)
      )
      val orCase = And(
        this.isOR,
        Not(l.isIGNORE),
        Not(r.isIGNORE)
      )
      val notCase = And(
        this.isNOT,
        Not(l.isIGNORE),
        r.isIGNORE
      )
      val ignoreCase = And(
        this.isIGNORE,
        l.isIGNORE,
        r.isIGNORE
      )
      val varCase = And(
        this.isVAR,
        l.isIGNORE,
        r.isIGNORE
      )

      val symmetryBreak = Implies(
        And(Not(l.isIGNORE), Not(r.isIGNORE)),
        LessEquals(l.nodeValue, r.nodeValue)
      )
      And(
        symmetryBreak,
        Or(andCase, orCase, notCase, ignoreCase, varCase)
      )
    }
  }

  object SymFunLeaf {
    def unapply(l: SymFunLeaf): Option[Variable] = {
      Some(l.v)
    }
  }

  class SymFunLeaf(
    val v: Variable,
    val possibleVars: Set[String]
  ) extends SymFunExpr {
    def nodeValue: Variable = v
    def children = List()
    def descendants = List()

    def nodeConsistency(): Expr = {
      Or(
        this.isVAR,
        this.isIGNORE
      )
    }
  }
}
