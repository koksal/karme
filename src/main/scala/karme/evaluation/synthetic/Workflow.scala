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
import karme.transformations.DistributionComparisonTest
import karme.transformations.NodePartialOrderByTrajectoryComparison
import karme.transformations.OracleNodePartialOrder
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
      nodePartialOrderType = opts.syntheticEvalOpts.nodePartialOrderType,
      distributionComparisonTest = DistributionComparisonTest.fromOptions(
        opts.inputTransformerOpts.distributionComparisonMethod),
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
    nodePartialOrderType: String,
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

    // build node partial order
    val nodePartialOrder = nodePartialOrderType match {
      case "oracle" => {
        new OracleNodePartialOrder(simulationGraph).partialOrdering
      }
      case "comparison" => {
        new NodePartialOrderByTrajectoryComparison(
          simulationGraph.V.toSeq,
          Seq(trajectory),
          distributionComparisonTest,
          distCompPValueThreshold
        ).partialOrdering
      }
    }

    // TODO add trajectory noise

    // remove nodes per deletion ratio
    // TODO? delete measurements, not graph nodes.
    val observedNodes = StateGraphPerturbation
      .deleteNodes(simulationGraph, nodeDeletionRatio).V

    // reconstruct graph
    val graphForSynthesis = StateGraphReconstruction.reconstructStateGraph(
      observedNodes, nodePartialOrder)

    // logging graphs
    if (false) {
      new StateGraphPlotter(reporter)
        .plotDirectedGraph(graphForSynthesis, "graph-for-synthesis")
    }

    // perform synthesis
    val synthesisResults = new Synthesizer(opts.synthOpts,
        reporter).synthesizeForPositiveHardConstraints(graphForSynthesis)

    // log synthesis results
    SynthesisResultLogger(synthesisResults, reporter.file("functions.txt"))

    // log hard partition sizes
    val hardPartitionSizeTuples = synthesisResults.toList map {
      case (key, results) => {
        Map("Gene" -> key, "Hard partition size" -> results.size)
      }
    }
    TSVUtil.saveTupleMapsWithOrderedHeaders(
      List("Gene", "Hard partition size"),
      hardPartitionSizeTuples,
      reporter.file("hard-partition-sizes-per-gene.tsv")
    )

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
      simulationGraph, nodePartialOrder)
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
