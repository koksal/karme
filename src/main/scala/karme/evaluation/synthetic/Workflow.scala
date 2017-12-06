package karme.evaluation.synthetic

import karme.ArgHandling
import karme.Experiments.Experiment
import karme.Opts
import karme.Reporter
import karme.evaluation.synthetic.examples.myeloid.MyeloidModel
import karme.evaluation.synthetic.examples.myeloid.MyeloidModelEvaluation
import karme.printing.SynthesisResultLogger
import karme.simulation.AsyncBooleanNetworkSimulation
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.SynthesisResult
import karme.synthesis.Synthesizer
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.transformations.AverageComparisonTest
import karme.transformations.DistributionComparisonTest
import karme.transformations.KolmogorovSmirnovTest
import karme.transformations.MinimumComparisonTest
import karme.transformations.RankSumTest
import karme.util.TSVUtil
import karme.visualization.graph.StateGraphPlotter

object Workflow {

  def main(args: Array[String]): Unit = {
    implicit val opts = ArgHandling.parseOptions(args)
    implicit val reporter = new Reporter(opts.reporterOpts)

    run(
      hiddenModel = MyeloidModel.makePLOSNetwork(),
      defaultInitialStates = Set(MyeloidModel.makeInitialState()),
      randomizedInitialStateInclusionRatio =
        opts.syntheticEvalOpts.randomizedInitialStateInclusionRatio,
      nodeDeletionRatio = opts.syntheticEvalOpts.nodeDeletionRatio,
      reconstructGraph = opts.syntheticEvalOpts.reconstructGraph,
      distributionComparisonTest = new KolmogorovSmirnovTest,
      distCompPValueThreshold =
        opts.inputTransformerOpts.distributionComparisonPValue,
      behaviorEvalFun = MyeloidModelEvaluation.evaluateModelBehavior
    )
  }

  def run(
    hiddenModel: Map[String, FunExpr],
    defaultInitialStates: Set[ConcreteBooleanState],
    randomizedInitialStateInclusionRatio: Option[Double],
    nodeDeletionRatio: Double,
    reconstructGraph: Boolean,
    distributionComparisonTest: DistributionComparisonTest,
    distCompPValueThreshold: Double,
    behaviorEvalFun: Map[String, FunExpr] => Map[String, Any]
  )(implicit reporter: Reporter, opts: Opts): Unit = {
    // modify initial states per extension ratio
    val initialStates = randomizedInitialStateInclusionRatio match {
      case Some(ratio) => {
        StateSetExtension
          .randomStateSet(defaultInitialStates.head.orderedKeys, ratio)
      }
      case None => defaultInitialStates
    }

    // run simulation
    val (simulationGraph, trajectory) = AsyncBooleanNetworkSimulation
      .simulateOneStepWithStateGraph(hiddenModel, initialStates)

    // (optionally) remove nodes per deletion ratio
    var graphForSynthesis = StateGraphPerturbation
      .deleteNodes(simulationGraph, nodeDeletionRatio)

    val experiment = Experiment(simulationGraph.V.toSeq.flatMap(_.measurements))

    // (optionally) reconstruct graph per flag
    if (reconstructGraph) {
      graphForSynthesis = StateGraphReconstruction.reconstructStateGraph(
        experiment, trajectory, distributionComparisonTest,
        distCompPValueThreshold)
    }

    // logging graphs
    if (false) {
      new StateGraphPlotter(reporter)
        .plotDirectedGraph(graphForSynthesis, "graph-for-synthesis")
    }

    // perform synthesis
    val synthesisResults = new Synthesizer(opts.synthOpts, reporter)
      .synthesizeForPositiveHardConstraints(graphForSynthesis)

    // log synthesis results
    SynthesisResultLogger(synthesisResults, reporter.file("functions.txt"))

    // evaluate simulation transition completeness w.r.t. all H-1 edges.
    val transitionToH1EdgeRatio = new SimulationGraphAnalysis()
      .transitionToAll1HammingRatio(simulationGraph)
    TSVUtil.saveOrderedTuples(
      List("transition to H-1 edge ratio"),
      List(List(transitionToH1EdgeRatio)),
      reporter.file("transition-to-h-1-edge-ratio.txt")
    )

    // evaluate edge orientation
    val orientationEvalTuples = new EdgeOrientationEval().evaluateOrientation(
      simulationGraph, Seq(trajectory), distributionComparisonTest,
      distCompPValueThreshold)
    TSVUtil.saveTupleMaps(
      Seq(orientationEvalTuples),
      reporter.file("orientation-eval.tsv")
    )

    // evaluate graph reconstruction
    TSVUtil.saveTupleMaps(
      Seq(new GraphComparison().diffGraphs(simulationGraph, graphForSynthesis)),
      reporter.file("graph-diff.tsv")
    )

    val resultCombinations = SynthesisResult.makeCombinations(synthesisResults)

    TSVUtil.saveOrderedTuples(
      List("# models"),
      List(List(resultCombinations.size)),
      reporter.file("number-of-models.tsv")
    )

    // evaluate behavior, function similarity, state spaces
    val behaviorEvalTuples = resultCombinations map { c =>
      behaviorEvalFun(c)
    }
    TSVUtil.saveTupleMaps(behaviorEvalTuples,
      reporter.file("behavior-eval.tsv"))

    val funSimilarityTuples = resultCombinations map { c =>
      FunSimilarityEval.evaluateFunSimilarity(hiddenModel, c)
    }
    TSVUtil.saveTupleMaps(funSimilarityTuples,
      reporter.file("function-similarity-eval.tsv"))

    val stateSpaceEvalTuples = resultCombinations map { c =>
      StateSpaceEval.compareStateSpaces(graphForSynthesis, c, initialStates)
    }
    TSVUtil.saveTupleMaps(stateSpaceEvalTuples,
      reporter.file("state-space-reproduction-eval.tsv"))

    // evaluate fixpoint reachability from perturbed initial states
    TSVUtil.saveTupleMaps(
      List(InitialStatePerturbationEval
        .fixpointSimilarityInitialVsPerturbedState(hiddenModel, initialStates)),
      reporter.file("hidden-model-init-state-vs-perturbed-state-fixpoints.tsv")
    )

    val initStatePerturbEvalTuples = resultCombinations map { c =>
      InitialStatePerturbationEval.fixpointSimilarityInitialVsPerturbedState(
        c, initialStates
      )
    }
    TSVUtil.saveTupleMaps(initStatePerturbEvalTuples,
      reporter.file(
        "inferred-models-init-state-vs-perturbed-state-fixpoints.tsv"))

    val hiddenVsInferredPerturbedStateEvalTuples = resultCombinations map { c =>
      InitialStatePerturbationEval.compareModelsForFixpointsFromPerturbedStates(
        hiddenModel, c, initialStates
      )
    }
    TSVUtil.saveTupleMaps(hiddenVsInferredPerturbedStateEvalTuples,
      reporter.file(
        "hidden-vs-inferred-models-perturbed-state-fixpoints.tsv"))

    // evaluate plain reachability from perturbed initial states
    TSVUtil.saveTupleMaps(
      List(InitialStatePerturbationEval
        .reachableStateSimilarityInitialVsPerturbedState(hiddenModel,
          initialStates)),
      reporter.file("hidden-model-init-state-vs-perturbed-state-reachable.tsv")
    )

    val hiddenVsInferredPerturbedStateReachability = resultCombinations map {
      c =>
        InitialStatePerturbationEval
          .compareModelsForReachableStatesFromPerturbedStates(
            hiddenModel, c, initialStates)
    }
    TSVUtil.saveTupleMaps(hiddenVsInferredPerturbedStateReachability,
      reporter.file(
        "hidden-vs-inferred-models-perturbed-state-reachable-states.tsv")
    )
  }

}
