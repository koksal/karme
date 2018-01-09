package karme.evaluation.synthetic

import karme.simulation.AsyncBooleanNetworkSimulation
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.util.CollectionUtil
import karme.util.MathUtil

object InitialStatePerturbationEval {

  val geneHeader = "Perturbation"
  val commonHeader = "Common"
  val missedHeader = "Missed"
  val spuriousHeader = "Spurious"
  val jaccardHeader = "Jaccard sim."

  val headers = List(
    geneHeader, commonHeader, missedHeader, spuriousHeader, jaccardHeader
  )

  def compareModelsForFixpointsFromPerturbedStates(
    hiddenModel: Map[String, FunExpr],
    inferredModel: Map[String, FunExpr],
    initStates: Set[ConcreteBooleanState]
  ): Seq[Map[String, Any]] = {
    val geneToSets = hiddenModel.keySet map { v =>
      val hiddenPerturbedFixpoints = fixpointsForPerturbedInitStates(
        hiddenModel, initStates, v
      )
      val inferredPerturbedFixpoints = fixpointsForPerturbedInitStates(
        inferredModel, initStates, v
      )
      v -> (hiddenPerturbedFixpoints, inferredPerturbedFixpoints)
    }
    setComparisonSummaries(geneToSets)
  }

  def compareModelsForReachableStatesFromPerturbedStates(
    hiddenModel: Map[String, FunExpr],
    inferredModel: Map[String, FunExpr],
    initStates: Set[ConcreteBooleanState]
  ): Seq[Map[String, Any]] = {
    val geneToSets = hiddenModel.keySet.map { v =>
      val hiddenPerturbedReachable = reachableStatesForPerturbedInitStates(
        hiddenModel, initStates, v
      )
      val inferredPerturbedReachable = reachableStatesForPerturbedInitStates(
        inferredModel, initStates, v
      )
      v -> (hiddenPerturbedReachable, inferredPerturbedReachable)
    }
    setComparisonSummaries(geneToSets)
  }

  def fixpointSimilarityInitialVsPerturbedState(
    model: Map[String, FunExpr],
    initStates: Set[ConcreteBooleanState]
  ): Seq[Map[String, Any]] = {
    val originalFixpoints = FixpointStates.findSimulationFixpoints(model,
      initStates)

    val geneToSets = model.keySet.map { v =>
      v -> (
        originalFixpoints,
        fixpointsForPerturbedInitStates(model, initStates, v)
      )
    }
    setComparisonSummaries(geneToSets)
  }

  def reachableStateSimilarityInitialVsPerturbedState(
    model: Map[String, FunExpr],
    initStates: Set[ConcreteBooleanState]
  ): Seq[Map[String, Any]] = {
    val originalReachableStates =
      AsyncBooleanNetworkSimulation.simulateOneStep(model, initStates)

    val geneToSets = model.keySet.map { v =>
      v -> (
        originalReachableStates,
        reachableStatesForPerturbedInitStates(model, initStates, v)
      )
    }
    setComparisonSummaries(geneToSets)
  }

  private def fixpointsForPerturbedInitStates(
    model: Map[String, FunExpr],
    initStates: Set[ConcreteBooleanState],
    varToPerturb: String
  ): Set[ConcreteBooleanState] = {
    val perturbedStates = initStates.map(s => s.replaceValue(varToPerturb,
      !s.value(varToPerturb)))
    FixpointStates.findSimulationFixpoints(model, perturbedStates)
  }

  private def reachableStatesForPerturbedInitStates(
    model: Map[String, FunExpr],
    initStates: Set[ConcreteBooleanState],
    varToPerturb: String
  ): Set[ConcreteBooleanState] = {
    val perturbedStates = initStates.map(s => s.replaceValue(varToPerturb,
      !s.value(varToPerturb)))
    AsyncBooleanNetworkSimulation.simulateOneStep(model, perturbedStates)
  }


  private def setComparisonSummaries[A](
    geneToSets: Iterable[(String, (Set[A], Set[A]))]
  ): Seq[Map[String, Any]] = {
    geneToSets.toList map {
      case (gene, (s1, s2)) => setComparisonSummary(gene, s1, s2)
    }
  }

  private def setComparisonSummary[A](
    gene: String, s1: Set[A], s2: Set[A]
  ): Map[String, Any] = {
    setComparisonSummary(s1, s2).updated(geneHeader, gene)
  }

  private def setComparisonSummary[A](
    s1: Set[A], s2: Set[A]
  ): Map[String, Any] = {
    val nbCommon = s1.intersect(s2).size
    val onlyIn1 = (s1 -- s2).size
    val onlyIn2 = (s2 -- s1).size
    val jaccardSim = CollectionUtil.jaccardSimilarity(s1, s2)
    Map(
      commonHeader -> nbCommon,
      missedHeader -> onlyIn1,
      spuriousHeader -> onlyIn2,
      jaccardHeader -> MathUtil.roundTo(2)(jaccardSim)
    )
  }
}
