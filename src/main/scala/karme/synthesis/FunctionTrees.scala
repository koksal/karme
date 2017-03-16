package karme.synthesis

import karme.synthesis.Transitions.ConcreteBooleanState
import karme.synthesis.Trees._
import karme.synthesis.traversal.BFS

object FunctionTrees {

  sealed trait FunExpr
  case class FunVar(id: String) extends FunExpr
  case class FunAnd(l: FunExpr, r: FunExpr) extends FunExpr
  case class FunOr(l: FunExpr, r: FunExpr) extends FunExpr
  case class FunNot(e: FunExpr) extends FunExpr

  def eval(fe: FunExpr, in: ConcreteBooleanState): Boolean = fe match {
    case FunVar(id) => in.value(id)
    case FunAnd(l, r) => eval(l, in) && eval(r, in)
    case FunOr(l, r) => eval(l, in) || eval(r, in)
    case FunNot(e) => !eval(e, in)
  }

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

  abstract class SymFunExpr {
    val possibleVars: Set[String]
    val orderedVariableOptions: List[String] = possibleVars.toList.sorted
    val encodingMapping = new EncodingMapping(orderedVariableOptions)

    def nodeValue: Variable
    def children: List[SymFunExpr]
    def descendants: List[SymFunExpr]

    def topLevelConsistency(): Expr = {
      And(
        Not(this.isIGNORE),
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
    def nbVariables(): Expr

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

      val onlyVarNegations = Implies(
        this.isNOT,
        this.l.isVAR
      )
      And(
        Or(andCase, orCase, notCase, ignoreCase, varCase),
        onlyVarNegations,
        symmetryBreaking()
      )
    }

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
      // if this is a var, then 1.
      // if it's a NOT, then carry from left child.
      // if a binary gate, then it's the sum of children's vars.
      // we don't care about IGNORE nodes.
      ITE(
        this.isVAR,
        IntLiteral(1),
        ITE(
          this.isNOT,
          l.nbVariables(),
          Plus(l.nbVariables(), r.nbVariables())
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

    def nodeConsistency(): Expr = {
      Or(
        this.isVAR,
        this.isIGNORE
      )
    }

    def symmetryBreaking(): Expr = {
      BooleanLiteral(true)
    }

    def nbVariables(): Expr = {
      // only used for VAR cases from parent nodes.
      IntLiteral(1)
    }
  }
}
