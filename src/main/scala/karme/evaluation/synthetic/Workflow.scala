package karme.evaluation.synthetic

import java.io.File

import karme.ArgHandling
import karme.Opts
import karme.Reporter
import karme.evaluation.synthetic.examples.myeloid.MyeloidModel
import karme.evaluation.synthetic.examples.myeloid.MyeloidModelEvaluation
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
import karme.util.TSVUtil
import karme.visualization.graph.StateGraphPlotter

import scala.util.Random

object Workflow {

  def main(args: Array[String]): Unit = {
    implicit val opts = ArgHandling.parseOptions(args)
    implicit val reporter = new Reporter(opts.reporterOpts)

    run(
      hiddenModel = MyeloidModel.makePLOSNetwork(),
      defaultInitialStates = Set(MyeloidModel.makeInitialState()),
      random = new Random(opts.syntheticEvalOpts.randomSeed),
      cellTrajectoryNoiseSigma =
        opts.syntheticEvalOpts.cellTrajectoryNoiseSigma,
      measurementNoiseProbability =
        opts.syntheticEvalOpts.measurementNoiseProbability,
      measurementDropProbability =
        opts.syntheticEvalOpts.measurementDropProbability,
      randomizedInitialStateInclusionRatio =
        opts.syntheticEvalOpts.randomizedInitialStateInclusionRatio,
      distributionComparisonTest = DistributionComparisonTest.fromOptions(
        opts.inputTransformerOpts.distributionComparisonMethod),
      distCompPValueThreshold =
        opts.inputTransformerOpts.distributionComparisonPValue
    )
  }

  def run(
    hiddenModel: Map[String, FunExpr],
    defaultInitialStates: Set[ConcreteBooleanState],
    random: Random,
    cellTrajectoryNoiseSigma: Double,
    measurementNoiseProbability: Double,
    measurementDropProbability: Double,
    randomizedInitialStateInclusionRatio: Option[Double],
    distributionComparisonTest: DistributionComparisonTest,
    distCompPValueThreshold: Double
  )(implicit reporter: Reporter, opts: Opts): Unit = {
    // modify initial states per extension ratio
    val initialStates = randomizedInitialStateInclusionRatio match {
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
      cellTrajectoryNoiseSigma,
      measurementNoiseProbability,
      measurementDropProbability
    ).generateExperiment(baseStateGraph, baseTrajectory)

    val nodes = StateGraphs.nodesFromExperiment(experiment)

    // build node partial order
    val nodePartialOrder = new NodePartialOrderByTrajectoryComparison(
      nodes.toSeq,
      Seq(trajectory),
      distributionComparisonTest,
      distCompPValueThreshold
    ).partialOrdering

    // evaluate simulation transition completeness w.r.t. all H-1 edges.
    val transitionToH1EdgeRatio = new SimulationGraphAnalysis()
      .transitionToAll1HammingRatio(baseStateGraph)
    TSVUtil.saveOrderedTuples(
      List("transition to H-1 edge ratio"),
      List(List(transitionToH1EdgeRatio)),
      reporter.file("transition-to-h-1-edge-ratio.txt")
    )

    // reconstruct graph
    val graphForSynthesis = new StateGraphReconstruction()
      .reconstructStateGraph(nodes, nodePartialOrder)

    // evaluate graph reconstruction
    TSVUtil.saveTupleMapsWithOrderedHeaders(
      GraphComparison.headers,
      Seq(GraphComparison.diffGraphs(baseStateGraph, graphForSynthesis)),
      reporter.file("graph-diff.tsv")
    )

    // logging graphs
    if (true) {
      new StateGraphPlotter(reporter)
        .plotDirectedGraph(graphForSynthesis, "graph-for-synthesis")
    }

    // perform synthesis
    val synthesisResults = new Synthesizer(opts.synthOpts,
        reporter).synthesizeForPositiveHardConstraints(graphForSynthesis)

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

    val models = SynthesisResult.makeCombinations(bestSynthesisResults)

    TSVUtil.saveOrderedTuples(
      List("# models"),
      List(List(models.size)),
      reporter.file("number-of-models.tsv")
    )

    TSVUtil.saveTupleMapsWithOrderedHeaders(
      ClassificationEval.headers,
      models map MyeloidModelEvaluation.evaluateWildTypeBehavior,
      reporter.file(s"stable-state-reachability-wildtype.tsv")
    )

    TSVUtil.saveTupleMapsWithOrderedHeaders(
      ClassificationEval.headers,
      models flatMap MyeloidModelEvaluation.evaluateKnockoutBehavior,
      reporter.file(s"stable-state-reachability-knockouts.tsv")
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

}
