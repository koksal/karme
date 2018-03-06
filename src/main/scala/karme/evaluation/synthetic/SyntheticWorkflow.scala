package karme.evaluation.synthetic

import karme.InputTransformerOpts
import karme.Reporter
import karme.SynthOpts
import karme.SyntheticEvalOpts
import karme.evaluation.synthetic.examples.myeloid.MyeloidModel
import karme.evaluation.synthetic.examples.myeloid.MyeloidModelEvaluation
import karme.evaluation.synthetic.expdesign.ExperimentGuideByReachableStates
import karme.graphs.StateGraphs
import karme.printing.LatexFunctionLogger
import karme.printing.SynthesisResultLogger
import karme.simulation.AsyncBooleanNetworkSimulation
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.SynthesisResult
import karme.synthesis.Synthesizer
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.transformations.DistributionComparisonTest
import karme.transformations.NodePartialOrderByTrajectoryComparison
import karme.util.CollectionUtil
import karme.util.FileUtil
import karme.util.TSVUtil
import karme.visualization.graph.StateGraphPlotter

import scala.util.Random

class SyntheticWorkflow(
  hiddenModel: Map[String, FunExpr],
  defaultInitialStates: Set[ConcreteBooleanState],
  targetStates: Set[ConcreteBooleanState],
  inputTransformerOpts: InputTransformerOpts,
  synthOpts: SynthOpts,
  syntheticEvalOpts: SyntheticEvalOpts
)(implicit reporter: Reporter) {

  val GUIDE_EXPERIMENTS = false
  val PLOT_STATE_GRAPHS = false

  val random = new Random(syntheticEvalOpts.randomSeed)

  def run(): Unit = {
    // modify initial states per extension ratio
    val initialStates = syntheticEvalOpts
      .randomizedInitialStateInclusionRatio match {
      case Some(ratio) => {
        new StateSetExtension(random)
          .randomStateSet(defaultInitialStates.head.orderedKeys, ratio)
      }
      case None => defaultInitialStates
    }

    // run simulation
    val (baseStateGraph, baseTrajectory) = AsyncBooleanNetworkSimulation
      .simulateOneStepWithTrimmedStateGraph(hiddenModel, initialStates)

    val timeTuples = baseStateGraph.V.map { v =>
      Map(
        "v" -> v.id,
        "times" -> v.measurements.map(m => baseTrajectory(m.id)).mkString(", ")
      )
    }
    TSVUtil.saveTupleMapsWithOrderedHeaders(
      List("v", "times"),
      timeTuples.toSeq.sortBy(row => row("v")),
      reporter.file("cell-trajectory-before-noise.tsv")
    )

    val (experiment, trajectory) = new SimulationToExperiment(random)(
      syntheticEvalOpts.cellTrajectoryNoiseSigma,
      syntheticEvalOpts.typeIErrorRatio,
      syntheticEvalOpts.typeIIErrorRatio
    ).generateExperiment(baseStateGraph, baseTrajectory)

    val nodes = StateGraphs.nodesFromExperiment(experiment)

    TSVUtil.saveTupleMapsWithOrderedHeaders(
      ClassificationEval.headers,
      Seq(
        GraphComparison.diffNodes(
          nodes.map(_.state),
          baseStateGraph.V.map(_.state)
        )
      ),
      reporter.file("sampled-states.tsv")
    )

    // build node partial order
    val nodePartialOrder = new NodePartialOrderByTrajectoryComparison(
      nodes.toSeq,
      Seq(trajectory),
      DistributionComparisonTest.fromOptions(
        inputTransformerOpts.distributionComparisonMethod),
      inputTransformerOpts.distributionComparisonPValue
    ).partialOrdering

    // reconstruct graph
    var graphForSynthesis = new StateGraphReconstruction()
      .reconstructStateGraph(nodes, nodePartialOrder)

    graphForSynthesis =
      AsyncBooleanNetworkSimulation.removeStatesNotReachingFixpoints(
        graphForSynthesis, targetStates)

    TSVUtil.saveTupleMapsWithOrderedHeaders(
      ClassificationEval.headers,
      Seq(
        GraphComparison.diffNodes(
          graphForSynthesis.V.map(_.state),
          baseStateGraph.V.map(_.state)
        )
      ),
      reporter.file("reconstructed-states.tsv")
    )

    // evaluate graph reconstruction
    TSVUtil.saveTupleMapsWithOrderedHeaders(
      GraphComparison.headers,
      Seq(GraphComparison.diffGraphs(baseStateGraph, graphForSynthesis)),
      reporter.file("graph-diff.tsv")
    )

    // logging graphs
    if (PLOT_STATE_GRAPHS) {
      new StateGraphPlotter(reporter)
        .plotDirectedGraph(
          graphForSynthesis,
          "graph-for-synthesis",
          nodeHighlightGroups = List(
            MyeloidModel.stableStates,
            baseStateGraph.V.map(_.state)
          )
        )
    }

    // perform synthesis
    val synthesisResults = new Synthesizer(synthOpts, reporter)
      .synthesizeForPositiveHardConstraints(graphForSynthesis)

    // log synthesis results
    SynthesisResultLogger(synthesisResults, reporter.file("functions.txt"))
    LatexFunctionLogger(synthesisResults, reporter.file("function-table.txt"))

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

    val bestSynthesisResults = pickBestResults(synthesisResults)

    val models = sampleModels(
      SynthesisResult.makeCombinations(bestSynthesisResults),
      random
    )

    if (GUIDE_EXPERIMENTS) {
      guideExperiment(models)
    }

    TSVUtil.saveOrderedTuples(
      List("# models"),
      List(List(models.size)),
      reporter.file("number-of-models.tsv")
    )

    TSVUtil.saveTupleMapsWithOrderedHeaders(
      ClassificationEval.headers,
      models map (m =>
        MyeloidModelEvaluation.evaluateWildTypeFixpoints(m, hiddenModel)),
      reporter.file("stable-states-wildtype.tsv")
    )

    TSVUtil.saveTupleMapsWithOrderedHeaders(
      ClassificationEval.headers,
      models map (m =>
        MyeloidModelEvaluation.evaluateWildTypeReachability(m, hiddenModel)),
      reporter.file("reachable-states-wildtype.tsv")
    )

    TSVUtil.saveTupleMapsWithOrderedHeaders(
      ClassificationEval.headers,
      models flatMap (m =>
        MyeloidModelEvaluation.evaluateKnockoutFixpoints(m, hiddenModel)),
      reporter.file("stable-states-knockouts.tsv")
    )

    TSVUtil.saveTupleMapsWithOrderedHeaders(
      ClassificationEval.headers,
      models flatMap (m =>
        MyeloidModelEvaluation.evaluateKnockoutReachability(m, hiddenModel)),
      reporter.file("reachable-states-knockouts.tsv")
    )

    TSVUtil.saveTupleMapsWithOrderedHeaders(
      FunSimilarityEval.behaviorSimilarityHeaders,
      models flatMap (m =>
        FunSimilarityEval.evaluateBehaviorSimilarity(m, hiddenModel,
          baseStateGraph.V.map(_.state))),
      reporter.file("function-behavior-similarity.tsv")
    )

    TSVUtil.saveTupleMapsWithOrderedHeaders(
      FunSimilarityEval.ioPairSimilarityHeaders,
      models map (m =>
        FunSimilarityEval.evaluateIOPairSimilarity(m, hiddenModel)),
      reporter.file("function-io-pair-similarity.tsv")
    )
  }

  def pickBestResults(
    results: Map[String, Set[SynthesisResult]]
  ): Map[String, SynthesisResult] = {
    val nonemptyResults = results filter (_._2.nonEmpty)
    nonemptyResults map {
      case (label, rs) => {
        label -> rs.maxBy(r => r.transitions.map(_.weight).sum)
      }
    }
  }

  def sampleModels(
    models: Seq[Map[String, FunExpr]],
    random: Random
  ): Seq[Map[String, FunExpr]] = {
    CollectionUtil.randomElements(random)(models, 10).toSeq
  }

  def guideExperiment(
    models: Seq[Map[String, FunExpr]]
  ): Unit = {
    val (exp, maxDist) = new ExperimentGuideByReachableStates(reporter)
      .mostDistinguishingExperiment(
        MyeloidModel.knockoutExperiments(),
        models,
        Set(MyeloidModel.makeInitialState())
      )

    FileUtil.writeToFile(
      reporter.file("most-distinguishing-experiment.txt"),
      s"${exp.knockoutVar}, $maxDist"
    )
  }

}

