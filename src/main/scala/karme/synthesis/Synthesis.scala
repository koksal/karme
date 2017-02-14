package karme.synthesis

import karme.synthesis.FunctionTrees._
import karme.synthesis.Transitions._
import karme.synthesis.Trees._

object Synthesis {

  val MAX_EXPRESSION_DEPTH = 3
  val MAX_NB_MODELS = None // Some(1)
  val FIND_UNSAT_CORES = false

  def synthesizeForAllLabels(
    positiveTransitions: Set[Transition],
    negativeTransitions: Set[Transition]
  ): Map[String, Set[FunExpr]] = {
    val allLabels = positiveTransitions.head.input.orderedKeys

    // group both type of transitions by label
    val labelToPosTrans = positiveTransitions.groupBy(_.label)
    val labelToNegTrans = negativeTransitions.groupBy(_.label)

    var labelToFunExpressions = Map[String, Set[FunExpr]]()
    for (label <- allLabels) {
      println()
      println(s"Synthesizing for ${label}")
      println("==========================")
      val funExprs = synthesizeForSingleLabel(
        hardTransitions = labelToNegTrans.getOrElse(label, Set.empty),
        softTransitions = labelToPosTrans.getOrElse(label, Set.empty),
        possibleVars = allLabels.toSet
      )
      labelToFunExpressions += label -> funExprs
    }

    labelToFunExpressions
  }

  private def synthesizeForSingleLabel(
    hardTransitions: Set[Transition],
    softTransitions: Set[Transition],
    possibleVars: Set[String]
  ): Set[FunExpr] = {
    // find a greedy partition of hard transitions such that each subset
    // is "maximally" consistent
    val partition = findGreedyTransitionPartition(hardTransitions,
      possibleVars)
    if (partition.size > 1) {
      println("Warning: Hard constraints are not consistent.")
      println(s"Partitioned hard examples into ${partition.size} set(s).")
      println(s"Subset sizes: ${partition.map(_.size).mkString(", ")}")
    }

    var allFunExprs = Set.empty[FunExpr]
    for (subset <- partition) {
      allFunExprs ++= synthesizeWithHardAndSoftTransitions(subset,
        softTransitions, possibleVars)
    }

    allFunExprs
  }

  private def findGreedyTransitionPartition(
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

  private def synthesizeWithHardAndSoftTransitions(
    hardTransitions: Set[Transition],
    softTransitions: Set[Transition],
    possibleVars: Set[String]
  ): List[FunExpr] = {
    var consistentSoftSet: Set[Transition] = Set.empty
    var currentExpressions: List[FunExpr] = Nil

    for (transition <- transitionsByDescendingWeight(softTransitions)) {
      val toTest = hardTransitions ++ consistentSoftSet + transition
      val expressions = synthesizeForMinDepth(toTest, possibleVars)
      if (expressions.nonEmpty) {
        consistentSoftSet += transition
        currentExpressions = expressions
      } else if (FIND_UNSAT_CORES) {
        println("Soft constraint inconsistent with current set:")
        println(transition)

        val core = minimalUnsatCore(Set(transition),
          hardTransitions ++ consistentSoftSet, possibleVars)
        println(s"Minimal core:")
        println(core.mkString("\n"))
        println()
      }
    }

    // If no soft transition is consistent with hard set, compute functions for
    // the hard set
    if (currentExpressions.isEmpty) {
      currentExpressions = synthesizeForMinDepth(hardTransitions, possibleVars)
    }

    val inconsistentSet = softTransitions -- consistentSoftSet
    val totalSoftWeightSum = softTransitions.map(_.weight).sum
    val inconsistentWeightSum = inconsistentSet.map(_.weight).sum
    println(s"There are ${inconsistentSet.size} / ${softTransitions.size} " +
      "inconsistent extensions (weights: " +
        s"${inconsistentWeightSum} / ${totalSoftWeightSum})")

    println(s"${currentExpressions.size} function(s) inferred with maximal " +
      s"extension set:")
    for (expr <- currentExpressions) {
      println(FunctionTrees.prettyString(expr))
    }

    currentExpressions
  }

  private def minimalUnsatCore(
    initialSet: Set[Transition],
    candidateSet: Set[Transition],
    possibleVars: Set[String]
  ): Set[Transition] = {
    assert(initialSet.nonEmpty)
    assert(candidateSet.nonEmpty)

    println("searching for unsat core.")

    var currentSet = initialSet
    var foundCore = false
    while (!foundCore) {
      val stepCandidateSet = candidateSet -- currentSet
      for (candidate <- stepCandidateSet; if !foundCore) {
        val toTest = currentSet + candidate
        val isUNSAT = synthesize(
          toTest, possibleVars, MAX_EXPRESSION_DEPTH).isEmpty
        if (isUNSAT) {
          foundCore = true
          currentSet = toTest
        }
      }
      if (!foundCore) {
        // add one (consistent) candidate to current set and repeat
        currentSet += stepCandidateSet.head
        println("Core not found, adding one candidate to current core:")
        println(currentSet.mkString("\n"))
        println()
        println("Function for current core:")
        println(FunctionTrees.prettyString(synthesizeForMinDepth(currentSet,
          possibleVars).head))
        println()
      }
    }

    assert(foundCore)
    currentSet
  }

  private def transitionsByDescendingWeight(ts: Iterable[Transition]) = {
    ts.toList.sortBy(_.weight).reverse
  }

  private def synthesizeForMinDepth(
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

  private def synthesize(
    transitions: Iterable[Transition],
    possibleVars: Set[String],
    depth: Int
  ): List[FunExpr] = {
    // create symbolic tree
    val symTree = mkFreshSymFunExpr(depth, possibleVars)
    val treeConsistent = symTree.topLevelConsistency()

    // applying symbolic tree to all transitions should yield output
    val transitionsValid = And(
      transitions.toList map (t => validTransition(symTree, t)): _*)

    val consistencyAndIO = And(treeConsistent, transitionsValid)
    findMinNbVars(symTree, consistencyAndIO) match {
      case Some(v) => {
        val minimalNbVars = Equals(
          symTree.nbVariables(),
          IntLiteral(v)
        )
        enumerateFunExpr(symTree, And(consistencyAndIO, minimalNbVars))
      }
      case None => {
        Nil
      }
    }

  }

  private def enumerateFunExpr(
    sfe: SymFunExpr,
    constraints: Expr
  ): List[FunExpr] = {
    def extract(model: Map[Identifier, Expr]): FunExpr =
      funExprValue(sfe, model)
    def symEq(fe: FunExpr): Expr = funExprEquals(sfe, fe)
    Enumeration.enumerate(extract, symEq, constraints, MAX_NB_MODELS)
  }

  private def findMinNbVars(
    sfe: SymFunExpr,
    constraints: Expr
  ): Option[Int] = {
    val objectiveVar = Trees.mkFreshIntVar("obj")
    val toMinimize = And(
      constraints,
      Equals(objectiveVar, sfe.nbVariables())
    )
    Optimization.minimize(objectiveVar.id, toMinimize)
  }

  private def mkFreshSymFunExpr(
    depth: Int, possibleVars: Set[String]
  ): SymFunExpr = {
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
    input: GenericState[_]
  ): (Variable, Expr) = {
    val res = mkFreshBooleanVar("res")

    val varCases = sf.orderedVariableOptions map { varName =>
      val varValue = input.value(varName) match {
        case b: Boolean => BooleanLiteral(b)
        case v: Variable => v
        case _ => sys.error("Unknown state type.")
      }
      Implies(
        sf.isVARLITERAL(varName),
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

  private def symbolicStateHasValue(
    symbolicState: SymBooleanState,
    concreteState: ConcreteBooleanState
  ): Expr = {
    val conj = symbolicState.orderedKeys map { key =>
      val symValue = symbolicState.value(key)
      val concValue = concreteState.value(key)
      Equals(symValue, BooleanLiteral(concValue))
    }
    And(conj: _*)
  }

}
