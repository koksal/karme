package karme.synthesis

import karme.Reporter
import karme.SynthOpts
import karme.evaluation.ReachabilityEvaluation
import karme.graphs.StateGraphs
import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.synthesis.FunctionTrees._
import karme.synthesis.Transitions._
import karme.synthesis.Trees._
import karme.transformations.TransitionProducer

class Synthesizer(opts: SynthOpts, reporter: Reporter) {

  def synthesizeForOptimalReachability(
    directedStateGraph: DirectedBooleanStateGraph,
    initialStates: Set[ConcreteBooleanState]
  ): Map[String, SynthesisResult] = {
    val (posTransitions, negTransitions) =
      producePositiveAndNegativeTransitions(directedStateGraph)

    val allResults = synthesizeFunctionsForAllTransitionSubsets(
      posTransitions, negTransitions)

    chooseSynthesisResultsForOptimalReachability(directedStateGraph,
      initialStates, allResults)
  }

  def producePositiveAndNegativeTransitions(
    directedStateGraph: DirectedBooleanStateGraph
  ): (Set[Transition], Set[Transition]) = {
    val stateNames = StateGraphs.namesFromStateGraph(directedStateGraph)
    val positiveTransitions = TransitionProducer.positiveTransitions(
      directedStateGraph)
    val negativeTransitions = TransitionProducer.negativeTransitions(
      directedStateGraph, stateNames)
    (positiveTransitions, negativeTransitions)
  }

  def chooseSynthesisResultsForOptimalReachability(
    g: DirectedBooleanStateGraph,
    initialStates: Set[ConcreteBooleanState],
    allResults: Map[String, Set[SynthesisResult]]
  ): Map[String, SynthesisResult] = {
    val observedStates = g.V.map(_.state)
    val reachabilityResult =
      ReachabilityEvaluation.chooseOptimalReachabilityResult(allResults,
      initialStates, observedStates, reporter)
    reachabilityResult.labelToResult
  }

  def synthesizeFunctionsForAllTransitionSubsets(
    positiveTransitions: Set[Transition],
    negativeTransitions: Set[Transition]
  ): Map[String, Set[SynthesisResult]] = {
    val allLabels = positiveTransitions.head.input.orderedKeys

    // group both type of transitions by label
    val labelToPosTrans = positiveTransitions.groupBy(_.label)
    val labelToNegTrans = negativeTransitions.groupBy(_.label)

    var labelToSynthesisResults = Map[String, Set[SynthesisResult]]()
    for (label <- allLabels) {
      println()
      println(s"Synthesizing for ${label}")
      println("==========================")
      val resultsForLabel = synthesizeForSingleLabel(
        hardTransitions = labelToNegTrans.getOrElse(label, Set.empty),
        softTransitions = labelToPosTrans.getOrElse(label, Set.empty),
        possibleVars = allLabels.toSet
      )
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
    if (partition.size > 1) {
      println("Warning: Hard constraints are not consistent.")
      println(s"Partitioned hard examples into ${partition.size} set(s).")
      println(s"Subset sizes: ${partition.map(_.size).mkString(", ")}")
    }

    var allResults = Set.empty[SynthesisResult]
    for (subset <- partition) {
      allResults ++= synthesizeWithHardAndSoftTransitions(subset,
        softTransitions, possibleVars)
    }

    allResults
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

  // assumes that hard transitions are consistent
  private def synthesizeWithHardAndSoftTransitions(
    hardTransitions: Set[Transition],
    softTransitions: Set[Transition],
    possibleVars: Set[String]
  ): Set[SynthesisResult] = {
    var unusedTransitions = softTransitions
    var currentResults = Set.empty[SynthesisResult]

    while (unusedTransitions.nonEmpty) {
      val nextBestUnused =
        transitionsByDescendingWeight(unusedTransitions).head

      // try to extend hard + next best with all soft constraints
      synthesizeWithMaximalSoftTransitions(hardTransitions + nextBestUnused,
        softTransitions - nextBestUnused, possibleVars) match {
        case Some(result @ SynthesisResult(ts, _)) => {
          // remove used ts from transitions to check
          unusedTransitions = unusedTransitions -- ts
          // add to current results
          currentResults += result
        }
        case None => {
          // the next best is not compatible with hard transitions
          unusedTransitions = unusedTransitions - nextBestUnused
        }
      }
    }

    // If no soft transition is consistent with hard set (or there are none),
    // compute functions for the hard set
    if (currentResults.isEmpty) {
      val fs = synthesizeForMinDepth(hardTransitions, possibleVars)
      assert(fs.nonEmpty)
      currentResults += SynthesisResult(hardTransitions, fs.toSet)
    }

    currentResults
  }

  private def synthesizeWithMaximalSoftTransitions(
    hardTransitions: Set[Transition],
    softTransitions: Set[Transition],
    possibleVars: Set[String]
  ): Option[SynthesisResult] = {
    val exprsForHardTrans = synthesizeForMinDepth(hardTransitions, possibleVars)

    if (exprsForHardTrans.nonEmpty) {
      // If the hard transitions are SAT, proceed with adding soft transitions.
      var currentSet = hardTransitions
      var currentExprs = exprsForHardTrans.toSet

      for (t <- transitionsByDescendingWeight(softTransitions)) {
        // try adding t to current set
        val toCheck = currentSet + t
        val exprsWithNewT = synthesizeForMinDepth(toCheck, possibleVars)
        if (exprsWithNewT.nonEmpty) {
          currentSet = toCheck
          currentExprs = exprsWithNewT.toSet
        }
      }

      Some(SynthesisResult(currentSet, currentExprs))
    } else {
      None
    }
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
        println(s"No core with ${currentSet.size} transitions, adding one " +
          s"candidate to current set:")
        println(currentSet.mkString("\n"))
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
    while (res.isEmpty && currDepth <= opts.maxExpressionDepth) {
      res = enumerateFunExprForMinNbVars(transitions, possibleVars, currDepth)
      currDepth += 1
    }
    res
  }

  def enumerateFunExprForMinNbVars(
    transitions: Iterable[Transition],
    possibleVars: Set[String],
    depth: Int
  ): List[FunExpr] = {
    // create symbolic tree
    val symTree = mkFreshSymFunExpr(depth, possibleVars)
    val treeConsistent = symTree.topLevelConsistencyWithFactorizedNegation()

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

object Synthesizer {
  // TODO move methods that do not access state here for testability
}
