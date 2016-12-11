package karme.synthesis

import karme.synthesis.FunctionTrees._
import karme.synthesis.Transitions._
import karme.synthesis.Trees._

object Synthesis {

  val MAX_EXPRESSION_DEPTH = 2

  def synthesizeForAllLabels(
    positiveTransitions: Set[Transition],
    negativeTransitions: Set[Transition]
  ): Unit = {
    val allLabels = positiveTransitions.head.input.orderedKeys

    // group both type of transitions by label
    val labelToPosTrans = positiveTransitions.groupBy(_.label)
    val labelToNegTrans = negativeTransitions.groupBy(_.label)

    for (label <- allLabels) {
      println()
      println(s"Synthesizing for ${label}")
      println("==========================")
      synthesizeForSingleLabel(
        labelToPosTrans.getOrElse(label, Set.empty),
        labelToNegTrans.getOrElse(label, Set.empty),
        allLabels.toSet
      )
    }
  }

  def synthesizeForSingleLabel(
    positiveTransitions: Set[Transition],
    negativeTransitions: Set[Transition],
    possibleVars: Set[String]
  ): Unit = {
    // find a greedy partition of positive transitions such that each subset
    // is "maximally" consistent
    val partition = findGreedyTransitionPartition(positiveTransitions,
      possibleVars)
    println(s"Partitioned positive examples into ${partition.size} sets")

    // aim to maximize use of negative transitions for each positive set
    for (subset <- partition) {
      extendTransitionSet(subset, negativeTransitions, possibleVars)
    }
  }

  def findGreedyTransitionPartition(
    transitions: Set[Transition],
    possibleVars: Set[String]
  ): Set[Set[Transition]] = {
    var remainingTransitions = transitions
    var partition: Set[Set[Transition]] = Set.empty

    while (remainingTransitions.nonEmpty) {
      var currentTransitionSet: Set[Transition] = Set.empty
      for (transition <- transitionsByDescendingWeight(remainingTransitions)) {
        val toTest = currentTransitionSet + transition
        val expressions = synthesizeForMinDepth(toTest, possibleVars)
        if (expressions.nonEmpty) {
          currentTransitionSet = toTest
        }
      }

      remainingTransitions = remainingTransitions -- currentTransitionSet
      partition += currentTransitionSet
    }

    partition
  }

  def extendTransitionSet(
    baseSet: Set[Transition],
    extensionSet: Set[Transition],
    possibleVars: Set[String]
  ): Unit = {
    println("Synthesizing with maximal soft constraints.")
    var consistentExtensionSet: Set[Transition] = Set.empty
    var currentExpressions: List[FunExpr] = Nil

    for (transition <- transitionsByDescendingWeight(extensionSet)) {
      val toTest = baseSet ++ consistentExtensionSet + transition
      val expressions = synthesizeForMinDepth(toTest, possibleVars)
      if (expressions.nonEmpty) {
        consistentExtensionSet += transition
        currentExpressions = expressions
      }
    }

    // If no extension transition is consistent with base set, compute
    // functions for the base set
    if (currentExpressions.isEmpty) {
      println("Base set could not be extended.")
      currentExpressions = synthesizeForMinDepth(baseSet, possibleVars)
    }

    val inconsistentSet = extensionSet -- consistentExtensionSet
    println(s"There are ${inconsistentSet.size} / ${extensionSet.size} " +
      "inconsistent extensions")

    println("Functions inferred with maximal extension set:")
    for (expr <- currentExpressions) {
      println(FunctionTrees.prettyString(expr))
    }
  }

  private def transitionsByDescendingWeight(ts: Iterable[Transition]) = {
    ts.toList.sortBy(_.weight).reverse
  }

  def synthesizeForMinDepth(
    transitions: Iterable[Transition],
    possibleVars: Set[String]
  ): List[FunExpr] = {
    var res = List[FunExpr]()
    var currDepth = 0
    while (res.isEmpty && currDepth <= MAX_EXPRESSION_DEPTH) {
      res = synthesize(transitions, possibleVars, currDepth)
      currDepth += 1
    }
    res
  }

  def synthesize(
    transitions: Iterable[Transition],
    possibleVars: Set[String],
    depth: Int
  ): List[FunExpr] = {
    // create symbolic tree
    val symTree = mkFreshSymFunExpr(depth, possibleVars)
    val treeConsistent = symTree.consistency()

    // applying symbolic tree to all transitions should yield output
    val transitionsValid = And(
      transitions.toList map (t => validTransition(symTree, t)): _*)

    val modelNbLimit = 10
    enumerateFunExpr(symTree,
      And(treeConsistent, transitionsValid), Some(modelNbLimit))
  }

  private def enumerateFunExpr(
    sfe: SymFunExpr,
    constraints: Expr,
    limit: Option[Int]
  ): List[FunExpr] = {
    def extract(model: Map[Identifier, Expr]): FunExpr =
      funExprValue(sfe, model)
    def symEq(fe: FunExpr): Expr = funExprEquals(sfe, fe)
    Enumeration.enumerate(extract, symEq, constraints, limit)
  }

  private def mkFreshSymFunExpr(depth: Int, possibleVars: Set[String]):
    SymFunExpr = {
    if (depth == 0) {
      new SymFunLeaf(Trees.mkFreshIntVar("leaf"), possibleVars)
    } else {
      val l = mkFreshSymFunExpr(depth - 1, possibleVars)
      val r = mkFreshSymFunExpr(depth - 1, possibleVars)
      new SymFunTree(l, Trees.mkFreshIntVar("interm"), r, possibleVars)
    }
  }

  private def evaluate(
    sf: SymFunExpr,
    input: AbsBooleanState[_]
  ): (Variable, Expr) = {
    val res = mkFreshBooleanVar("res")

    val size = input.orderedKeys.size
    val varCases = sf.prots map { prot =>
      val varValue = input match {
        case cbs: ConcreteBooleanState => BooleanLiteral(cbs(prot))
        case cpbs: ConcreteProbabilisticBooleanState =>
          BooleanLiteral(cpbs(prot).value)
        case sbs: SymBooleanState => sbs(prot)
      }
      Implies(
        sf.isPROT(prot),
        Equals(res, varValue)
      )
    }

    sf match {
      case SymFunTree(l, _, r) => {
        val (lres, lcons) = evaluate(l, input)
        val (rres, rcons) = evaluate(r, input)
        val andCase = Implies(
          sf.isAND,
          Equals(res, And(lres, rres))
        )
        val orCase = Implies(
          sf.isOR,
          Equals(res, Or(lres, rres))
        )
        val notCase = Implies(
          sf.isNOT,
          Equals(res, Not(lres))
        )
        val localCons = And(List(andCase, orCase, notCase) ::: varCases: _*)
        (res, And(localCons, lcons, rcons))
      }
      case SymFunLeaf(_) => {
        (res, And(varCases: _*))
      }
    }

  }

  private def validTransition(sf: SymFunExpr, transition: Transition): Expr = {
    val (res, evaluationConstraint) = evaluate(sf, transition.input)
    And(
      Equals(res, BooleanLiteral(transition.output)),
      evaluationConstraint
    )
  }

  private def funExprValue(e: SymFunExpr, m: Map[Identifier, Expr]): FunExpr = {
    e match {
      case SymFunTree(l, v, r) => {
        m(v.id) match {
          case x if x == e.encodingMapping.AND_NODE =>
            FunAnd(funExprValue(l, m), funExprValue(r, m))
          case x if x == e.encodingMapping.OR_NODE  =>
            FunOr(funExprValue(l, m), funExprValue(r, m))
          case x if x == e.encodingMapping.NOT_NODE =>
            FunNot(funExprValue(l, m))
          case x if x == e.encodingMapping.IGNORE_NODE =>
            throw new Exception("not reachable")
          case IntLiteral(i) =>
            FunVar(e.encodingMapping.VAR_NAME(i))
        }
      }
      case SymFunLeaf(v) => {
        m(v.id) match {
          case x if x == e.encodingMapping.IGNORE_NODE =>
            throw new Exception("not reachable")
          case IntLiteral(i) =>
            FunVar(e.encodingMapping.VAR_NAME(i))
        }
      }
    }
  }

  // Returns a formula stating that sfe equals the concrete fe
  private def funExprEquals(sfe: SymFunExpr, fe: FunExpr): Expr = {
    fe match {
      case FunVar(id) => {
        Equals(sfe.nodeValue, sfe.encodingMapping.VAR_NODE(id))
      }
      case FunAnd(l, r) => {
        sfe match {
          case SymFunTree(symL, _, symR) => {
            And(
              sfe.isAND,
              funExprEquals(symL, l),
              funExprEquals(symR, r)
            )
          }
          case SymFunLeaf(_) => sys.error("Insufficient symbolic tree height")
        }
      }
      case FunOr(l, r) => {
        sfe match {
          case SymFunTree(symL, _, symR) => {
            And(
              sfe.isOR,
              funExprEquals(symL, l),
              funExprEquals(symR, r)
            )
          }
          case SymFunLeaf(_) => sys.error("Insufficient symbolic tree height")
        }
      }
      case FunNot(e) => {
        sfe match {
          case SymFunTree(symL, _, _) => {
            And(
              sfe.isNOT,
              funExprEquals(symL, e)
            )
          }
          case SymFunLeaf(_) => sys.error("Insufficient symbolic tree height")
        }
      }
    }

  }

}
