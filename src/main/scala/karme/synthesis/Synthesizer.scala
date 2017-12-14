package karme.synthesis

import karme.evaluation.FunExprSimilarity
import karme.{Reporter, SynthOpts}
import karme.graphs.StateGraphs
import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.synthesis.FunctionTrees._
import karme.synthesis.Transitions._
import karme.synthesis.Trees._
import karme.transformations.TransitionProducer
import karme.util.TSVUtil
import karme.util.TimingUtil

class Synthesizer(opts: SynthOpts, reporter: Reporter) {

  def synthesizeForPositiveHardConstraints(
    graph: DirectedBooleanStateGraph
  ): Map[String, Set[SynthesisResult]] = {
    if (graph.V.isEmpty) {
      reporter.log("Graph is empty, skipping synthesis.")
      Map.empty
    } else {
      val (posTrans, negTrans) =
        TransitionProducer.producePositiveAndNegativeTransitions(graph)

      synthesizeFunctionsForAllTransitionSubsets(posTrans, negTrans,
        StateGraphs.namesFromStateGraph(graph))
    }
  }

  private def synthesizeFunctionsForAllTransitionSubsets(
    positiveTransitions: Set[Transition],
    negativeTransitions: Set[Transition],
    labels: Set[String]
  ): Map[String, Set[SynthesisResult]] = {
    // group both type of transitions by label
    val labelToPosTrans = positiveTransitions.groupBy(_.label)
    val labelToNegTrans = negativeTransitions.groupBy(_.label)

    var labelToSynthesisResults = Map[String, Set[SynthesisResult]]()
    for (label <- labels) {
      reporter.debug(s"Synthesizing for ${label}")
      reporter.debug("==========================")

      val resultsForLabel = TimingUtil.log(
        label, reporter.file("synthesis-times-in-milliseconds.tsv")
      ) {
        synthesizeForSingleLabel(
          hardTransitions = labelToPosTrans.getOrElse(label, Set.empty),
          softTransitions = labelToNegTrans.getOrElse(label, Set.empty),
          possibleVars = labels
        )
      }
      labelToSynthesisResults += label -> resultsForLabel
    }

    labelToSynthesisResults
  }

  private def synthesizeForSingleLabel(
    hardTransitions: Set[Transition],
    softTransitions: Set[Transition],
    possibleVars: Set[String]
  ): Set[SynthesisResult] = {
    // find a greedy partition of hard transitions such that each subset
    // is "maximally" consistent
    val partition = findGreedyTransitionPartition(hardTransitions,
      possibleVars)

    reporter.debug(s"# hard transitions: ${hardTransitions.size}")
    reporter.debug(s"# soft transitions: ${softTransitions.size}")

    var allResults = Set.empty[SynthesisResult]
    for (subset <- partition) {
      allResults += synthesizeWithMaximalSoftTransitions(subset,
        softTransitions, possibleVars)
    }

    allResults
  }

  private def findGreedyTransitionPartition(
    transitions: Set[Transition],
    possibleVars: Set[String]
  ): Set[Set[Transition]] = {
    // first try all transitions eagerly
    if (enumerateForMinDepth(transitions, possibleVars).nonEmpty) {
      reporter.debug("All hard constraints are consistent.")
      Set(transitions)
    } else {
      reporter.debug("Hard constraints are not consistent.")
      var remainingTransitions = transitions
      var partition: Set[Set[Transition]] = Set.empty

      while (remainingTransitions.nonEmpty) {
        var currentTransitionSet: Set[Transition] = Set.empty
        for (transition <- transitionsByDescendingWeight(remainingTransitions)) {
          val toTest = currentTransitionSet + transition
          if (enumerateForMinDepth(toTest, possibleVars).nonEmpty) {
            currentTransitionSet = toTest
          }
        }

        remainingTransitions = remainingTransitions -- currentTransitionSet
        partition += currentTransitionSet
      }

      partition
    }
  }

  private def synthesizeWithMaximalSoftTransitions(
    hardTransitions: Set[Transition],
    softTransitions: Set[Transition],
    possibleVars: Set[String]
  ): SynthesisResult = {
    val exprsForAllTrans = enumerateForMinDepthAndMinNbVars(
      hardTransitions ++ softTransitions, possibleVars)

    if (exprsForAllTrans.nonEmpty) {
      reporter.debug("Found expressions with eager check.")
      SynthesisResult(
        hardTransitions ++ softTransitions,
        exprsForAllTrans.toSet
      )
    } else {
      reporter.debug("Did not find expressions with eager check.")

      var currentSet = hardTransitions
      var remainingSoftTrans = softTransitions

      while (remainingSoftTrans.nonEmpty) {
        println(s"There are ${remainingSoftTrans.size} soft transitions " +
          s"remaining.")
        val (maximalSoftConsPrefix, firstIncompatibleTrans) =
          findMaximalSoftConstraintPrefix(currentSet,
            remainingSoftTrans, possibleVars)

        currentSet ++= maximalSoftConsPrefix

        remainingSoftTrans --= maximalSoftConsPrefix
        remainingSoftTrans --= firstIncompatibleTrans
      }

      val finalExprs = FunExprSimilarity.findNonRedundantSet(
        enumerateForMinDepthAndMinNbVars(currentSet, possibleVars).toSet)
      SynthesisResult(currentSet, finalExprs)
    }
  }

  private def findMaximalSoftConstraintPrefix(
    hardTransitions: Set[Transition],
    softTransitions: Set[Transition],
    possibleVars: Set[String]
  ): (Set[Transition], Option[Transition]) = {
    assert(softTransitions.nonEmpty)

    val sortedSoftTrans = transitionsByDescendingWeight(softTransitions)

    var min = 0
    var max = sortedSoftTrans.size - 1

    var lastSat: Option[Int] = None

    while (max >= min) {
      val pivot = (max + min) / 2
      reporter.debug(s"Testing ${pivot + 1} / ${sortedSoftTrans.size} first " +
        "soft constraints...")
      val toCheck = hardTransitions ++ (sortedSoftTrans.take(pivot + 1))

      if (enumerateForMinDepth(toCheck, possibleVars).nonEmpty) {
        min = pivot + 1
        lastSat = Some(pivot)
      } else {
        max = pivot - 1
      }
    }

    val firstIncompatible: Option[Transition] = lastSat match {
      case Some(i) => {
        if (i < sortedSoftTrans.size - 1) {
          Some(sortedSoftTrans(i + 1))
        } else {
          reporter.debug("No inconsistent transition found in binary search.")
          None
        }
      }
      case None => {
        Some(sortedSoftTrans.head)
      }
    }

    val prefix = lastSat match {
      case Some(i) => {
        sortedSoftTrans.take(i + 1)
      }
      case None => {
        Nil
      }
    }

    reporter.debug(
      s"Found maximal soft constraint prefix of size: ${prefix.size}")
    (prefix.toSet, firstIncompatible)
  }

  private def minimalUnsatCore(
    initialSet: Set[Transition],
    candidateSet: Set[Transition],
    possibleVars: Set[String]
  ): Set[Transition] = {
    assert(initialSet.nonEmpty)
    assert(candidateSet.nonEmpty)

    reporter.debug("searching for unsat core.")

    var currentSet = initialSet
    var foundCore = false
    while (!foundCore) {
      val stepCandidateSet = candidateSet -- currentSet
      for (candidate <- stepCandidateSet; if !foundCore) {
        val toTest = currentSet + candidate
        val isUNSAT = enumerateFunExprForMinNbVars(
          toTest, possibleVars, opts.maxExpressionDepth).isEmpty
        if (isUNSAT) {
          foundCore = true
          currentSet = toTest
        }
      }
      if (!foundCore) {
        // add one (consistent) candidate to current set and repeat
        currentSet += stepCandidateSet.head
        reporter.debug(s"No core with ${currentSet.size} transitions, adding " +
          "one candidate to current set:")
        reporter.debug(currentSet.mkString("\n"))
      }
    }

    assert(foundCore)
    currentSet
  }

  private def transitionsByDescendingWeight(ts: Iterable[Transition]) = {
    ts.toList.sortBy(_.weight).reverse
  }

  private def enumerateForMinDepthAndMinNbVars(
    transitions: Iterable[Transition],
    possibleVars: Set[String]
  ): Iterator[FunExpr] = {
    var res: Iterator[FunExpr] = Iterator.empty
    var currDepth = 0
    while (res.isEmpty && currDepth <= opts.maxExpressionDepth) {
      res = enumerateFunExprForMinNbVars(transitions, possibleVars, currDepth)
      currDepth += 1
    }
    res
  }

  private def enumerateForMinDepth(
    transitions: Iterable[Transition],
    possibleVars: Set[String]
  ): Iterator[FunExpr] = {
    var res: Iterator[FunExpr] = Iterator.empty
    var currDepth = 0
    while (res.isEmpty && currDepth <= opts.maxExpressionDepth) {
      res = enumerateFunExpr(transitions, possibleVars, currDepth)
      currDepth += 1
    }
    res
  }

  def enumerateFunExprForMinNbVars(
    transitions: Iterable[Transition],
    possibleVars: Set[String],
    depth: Int
  ): Iterator[FunExpr] = {
    val (symTree, symTreeCons) = makeSymbolicFunExprAndConstraints(
      transitions, possibleVars, depth)

    findMinNbVars(symTree, symTreeCons) match {
      case Some(v) => {
        val minimalNbVars = Equals(
          symTree.nbVariables(),
          IntLiteral(v)
        )
        enumerateFunExpr(symTree, And(symTreeCons, minimalNbVars))
      }
      case None => {
        Iterator.empty
      }
    }
  }

  def enumerateFunExpr(
    transitions: Iterable[Transition],
    possibleVars: Set[String],
    depth: Int
  ): Iterator[FunExpr] = {
    val (symTree, symTreeCons) = makeSymbolicFunExprAndConstraints(
      transitions, possibleVars, depth)

    enumerateFunExpr(symTree, symTreeCons)
  }

  private def makeSymbolicFunExprAndConstraints(
    transitions: Iterable[Transition],
    possibleVars: Set[String],
    depth: Int
  ): (SymFunExpr, Expr) = {
    // create symbolic tree
    val symTree = mkFreshSymFunExpr(depth, possibleVars)
    val treeConsistent = symTree.topLevelConsistencyWithArbitraryStructure()

    // applying symbolic tree to all transitions should yield output
    val transitionsValid = And(
      transitions.toList map (t => validTransition(symTree, t)): _*)

    (symTree, And(treeConsistent, transitionsValid))
  }

  private def enumerateFunExpr(
    sfe: SymFunExpr,
    constraints: Expr
  ): Iterator[FunExpr] = {
    def extract(model: Map[Identifier, Expr]): FunExpr =
      funExprValue(sfe, model)
    def symEq(fe: FunExpr): Expr = funExprEquals(sfe, fe)
    Enumeration.enumerate(extract, symEq, constraints, opts.maxNbModels)
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

    val trueCase = Implies(
      sf.isTRUE,
      Equals(res, BooleanLiteral(true))
    )
    val falseCase = Implies(
      sf.isFALSE,
      Equals(res, BooleanLiteral(false))
    )

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

        val localCons = And(
          List(andCase, orCase, notCase, trueCase, falseCase) ::: varCases: _*)
        (res, And(localCons, lcons, rcons))
      }
      case SymFunLeaf(_) => {
        (res, And(List(trueCase, falseCase) ::: varCases: _*))
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
          case x if x == e.encodingMapping.TRUE_NODE =>
            FunConst(true)
          case x if x == e.encodingMapping.FALSE_NODE =>
            FunConst(false)
          case x if x == e.encodingMapping.IGNORE_NODE =>
            throw new Exception("not reachable")
          case IntLiteral(i) =>
            FunVar(e.encodingMapping.VAR_NAME(i))
        }
      }
      case SymFunLeaf(v) => {
        m(v.id) match {
          case x if x == e.encodingMapping.TRUE_NODE =>
            FunConst(true)
          case x if x == e.encodingMapping.FALSE_NODE =>
            FunConst(false)
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
      case FunConst(true) => {
        Equals(sfe.nodeValue, sfe.encodingMapping.TRUE_NODE)
      }
      case FunConst(false) => {
        Equals(sfe.nodeValue, sfe.encodingMapping.FALSE_NODE)
      }
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

object Synthesizer {
  // TODO move methods that do not access state here for testability
}
