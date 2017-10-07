package karme.synthesis

import karme.synthesis.Transitions.ConcreteBooleanState
import karme.synthesis.Trees._
import karme.synthesis.traversal.BFS

object FunctionTrees {

  sealed trait FunExpr
  case class FunConst(v: Boolean) extends FunExpr
  case class FunVar(id: String) extends FunExpr
  case class FunAnd(l: FunExpr, r: FunExpr) extends FunExpr
  case class FunOr(l: FunExpr, r: FunExpr) extends FunExpr
  case class FunNot(e: FunExpr) extends FunExpr

  def eval(fe: FunExpr, in: ConcreteBooleanState): Boolean = fe match {
    case FunConst(v) => v
    case FunVar(id) => in.value(id)
    case FunAnd(l, r) => eval(l, in) && eval(r, in)
    case FunOr(l, r) => eval(l, in) || eval(r, in)
    case FunNot(e) => !eval(e, in)
  }

  def replaceVar(
    fe: FunExpr, id: String, newExpr: FunExpr
  ): FunExpr = fe match {
    case FunConst(_) => fe
    case FunVar(`id`) => newExpr
    case FunVar(_) => fe
    case FunAnd(l, r) =>
      FunAnd(replaceVar(l, id, newExpr), replaceVar(r, id, newExpr))
    case FunOr(l, r) =>
      FunOr(replaceVar(l, id, newExpr), replaceVar(r, id, newExpr))
    case FunNot(e) => FunNot(replaceVar(e, id, newExpr))
  }

  def simplify(fe: FunExpr): FunExpr = fe match {
    case FunAnd(FunConst(true), r) => simplify(r)
    case FunAnd(l, FunConst(true)) => simplify(l)
    case FunAnd(l, r) => FunAnd(simplify(l), simplify(r))
    case FunOr(FunConst(false), r) => simplify(r)
    case FunOr(l, FunConst(false)) => simplify(l)
    case FunOr(l, r) => FunOr(simplify(l), simplify(r))
    case FunNot(FunNot(e)) => simplify(e)
    case FunNot(FunAnd(l, r)) => FunOr(simplify(FunNot(l)), simplify(FunNot(r)))
    case FunNot(FunOr(l, r)) => FunAnd(simplify(FunNot(l)), simplify(FunNot(r)))
    case FunNot(e) => FunNot(simplify(e))
    case _ => fe
  }

  def leq(e1: FunExpr, e2: FunExpr): Boolean = (e1, e2) match {
    case (FunConst(v1), FunConst(v2)) => v1 <= v2
    case (FunConst(_), _) => true
    case (FunVar(v1), FunVar(v2)) => v1 <= v2
    case (FunVar(_), _) => true
    case (FunNot(l), FunNot(r)) => leq(l, r)
    case (FunNot(_), _) => true
    case (FunAnd(l1, r1), FunAnd(l2, r2)) => leq(r1, l2)
    case (FunAnd(_, _), _) => true
    case (FunOr(l1, r1), FunOr(l2, r2)) => leq(r1, l2)
    case _ => !leq(e2, e1)
  }

  def canonicalize(fe: FunExpr): FunExpr = fe match {
    case FunNot(e) => FunNot(canonicalize(e))
    case FunAnd(l, r) => {
      val cl = canonicalize(l)
      val cr = canonicalize(r)
      if (leq(cl, cr)) FunAnd(cl, cr) else FunAnd(cr, cl)
    }
    case FunOr(l, r) => {
      val cl = canonicalize(l)
      val cr = canonicalize(r)
      if (leq(cl, cr)) FunOr(cl, cr) else FunOr(cr, cl)
    }
    case _ => fe
  }

  def collectIdentifiers(fe: FunExpr): Set[String] = fe match {
    case FunConst(_) => Set()
    case FunVar(id) => Set(id)
    case FunAnd(l, r) => collectIdentifiers(l) ++ collectIdentifiers(r)
    case FunOr(l, r) => collectIdentifiers(l) ++ collectIdentifiers(r)
    case FunNot(e) => collectIdentifiers(e)
  }

  def collectIdentifiersWithSigns(
    fe: FunExpr,
    currentContextIsPositive: Boolean = true
  ): Set[(String, Boolean)] = fe match {
    case FunConst(_) => Set()
    case FunVar(id) => Set((id, currentContextIsPositive))
    case FunAnd(l, r) =>
      collectIdentifiersWithSigns(l, currentContextIsPositive) ++
        collectIdentifiersWithSigns(r, currentContextIsPositive)
    case FunOr(l, r) =>
      collectIdentifiersWithSigns(l, currentContextIsPositive) ++
        collectIdentifiersWithSigns(r, currentContextIsPositive)
    case FunNot(e) => collectIdentifiersWithSigns(e, !currentContextIsPositive)
  }

  abstract class SymFunExpr {
    val possibleVars: Set[String]
    val orderedVariableOptions: List[String] = possibleVars.toList.sorted
    val encodingMapping = new EncodingMapping(orderedVariableOptions)

    def nodeValue: Variable
    def children: List[SymFunExpr]
    def descendants: List[SymFunExpr]

    def topLevelConsistencyWithArbitraryStructure(): Expr = {
      And(
        Not(this.isIGNORE),
        this.localNodeConsistency(canBeNegation = true),
        And(this.descendants.map(_.localNodeConsistency(canBeNegation = true)): _*)
      )
    }

    def localNodeConsistency(canBeNegation: Boolean): Expr
    def nbVariables(): Expr

    def isTRUE: Expr = Equals(this.nodeValue, encodingMapping.TRUE_NODE)
    def isFALSE: Expr = Equals(this.nodeValue, encodingMapping.FALSE_NODE)
    def isCONST: Expr = Or(this.isTRUE, this.isFALSE)

    def isAND: Expr = Equals(this.nodeValue, encodingMapping.AND_NODE)
    def isOR: Expr = Equals(this.nodeValue, encodingMapping.OR_NODE)
    def isNOT: Expr = Equals(this.nodeValue, encodingMapping.NOT_NODE)
    def isIGNORE: Expr = Equals(this.nodeValue, encodingMapping.IGNORE_NODE)
    def isVARLITERAL(name: String) =
      Equals(this.nodeValue, encodingMapping.VAR_NODE(name))

    def isVAR: Expr = {
      val v = this.nodeValue
      val disj = encodingMapping.VAR_NODE_RANGE map { vn =>
        Equals(v, vn)
      }
      Or(disj.toList: _*)
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

    def localNodeConsistency(canBeNegation: Boolean): Expr = {
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
      val notCase = if (canBeNegation) {
        And(
          this.isNOT,
          Not(l.isIGNORE),
          r.isIGNORE
        )
      } else {
        BooleanLiteral(false)
      }
      val constCase = And(
        this.isCONST,
        l.isIGNORE,
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

      And(
        Or(andCase, orCase, notCase, constCase, ignoreCase, varCase),
        symmetryBreaking()
      )
    }

    // TODO adapt to unbalanced symbolic tree structures if needed
    private def symmetryBreaking(): Expr = {
      Implies(
        Or(this.isAND, this.isOR),
        lexicographicalLTE(BFS.order(l), BFS.order(r))
      )
    }

    // TODO move somewhere else.
    private def lexicographicalLTE(
      ls: Seq[SymFunExpr], rs: Seq[SymFunExpr]
    ): Expr = (ls.toList, rs.toList) match {
      case (l :: lRest, r :: rRest) => {
        Or(
          LessThan(l.nodeValue, r.nodeValue),
          And(
            Equals(l.nodeValue, r.nodeValue),
            lexicographicalLTE(lRest, rRest)
          )
        )
      }
      case (Nil, Nil) => {
        BooleanLiteral(true)
      }
      case _ => sys.error("Sequences of unequal length.")
    }

    def nbVariables(): Expr = {
      // if this is a const, then 0.
      // if this is a var, then 1.
      // if it's a NOT, then carry from left child.
      // if a binary gate, then it's the sum of children's vars.
      // we don't care about IGNORE nodes.
      ITE(
        this.isCONST,
        IntLiteral(0),
        ITE(
          this.isVAR,
          IntLiteral(1),
          ITE(
            this.isNOT,
            l.nbVariables(),
            Plus(l.nbVariables(), r.nbVariables())
          )
        )
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

    def localNodeConsistency(canBeNegation: Boolean): Expr = {
      Or(
        this.isCONST,
        this.isVAR,
        this.isIGNORE
      )
    }

    def nbVariables(): Expr = {
      ITE(
        this.isCONST,
        IntLiteral(0),
        // VAR case, used from parent nodes.
        IntLiteral(1)
      )
    }
  }
}
