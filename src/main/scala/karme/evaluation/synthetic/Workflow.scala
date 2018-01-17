package karme.evaluation.synthetic

import karme.ArgHandling
import karme.Opts
import karme.Reporter
import karme.evaluation.synthetic.examples.myeloid.MyeloidModel
import karme.evaluation.synthetic.examples.myeloid.MyeloidModelEvaluation
import karme.printing.LatexFunctionLogger
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
    random: Random,
    cellTrajectoryNoiseSigma: Double,
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
          .randomStateSet(defaultInitialStates.head.orderedKeys, ratio, random)
      }
      case None => defaultInitialStates
    }

    // run simulation
    var (simulationGraph, trajectory) = AsyncBooleanNetworkSimulation
      .simulateOneStepWithTrimmedStateGraph(hiddenModel, initialStates)

    trajectory = new CellTrajectoryNoise(cellTrajectoryNoiseSigma)
      .addNoise(trajectory)

    val timeTuples = simulationGraph.V.map { v =>
      Map(
        "v" -> v.id,
        "times" -> v.measurements.map(m => trajectory(m.id)).mkString(", ")
      )
    }
    TSVUtil.saveTupleMapsWithOrderedHeaders(
      List("v", "times"),
      timeTuples.toSeq.sortBy(row => row("v")),
      reporter.file("simulated-trajectory.txt")
    )

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

    // remove nodes per deletion ratio
    // TODO? delete measurements, not graph nodes.
    val observedNodes = StateGraphPerturbation
      .deleteNodes(simulationGraph, nodeDeletionRatio, random).V

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
    TSVUtil.saveTupleMapsWithOrderedHeaders(
      EdgeOrientationEval.headers,
      Seq(orientationEvalTuples),
      reporter.file("orientation-eval.tsv")
    )

    // reconstruct graph
    val graphForSynthesis = new StateGraphReconstruction()
      .reconstructStateGraph(observedNodes, nodePartialOrder)

    // evaluate graph reconstruction
    TSVUtil.saveTupleMapsWithOrderedHeaders(
      GraphComparison.headers,
      Seq(GraphComparison.diffGraphs(simulationGraph, graphForSynthesis)),
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

    val resultCombinations = SynthesisResult.makeCombinations(synthesisResults)

    TSVUtil.saveOrderedTuples(
      List("# models"),
      List(List(resultCombinations.size)),
      reporter.file("number-of-models.tsv")
    )

    // evaluate behavior, function similarity, state spaces
    val behaviorEvalTuples = resultCombinations map { c =>
      List(behaviorEvalFun(c))
    }
    printPerModelTuples(
      MyeloidModelEvaluation.headers,
      Nil,
      behaviorEvalTuples,
      "stable-state-reachability-across-conditions"
    )

    val funSimilarityPerModel = resultCombinations map { c =>
      FunSimilarityEval.evaluateFunSimilarity(hiddenModel, c)
    }
    printPerModelTuples(
      FunSimilarityEval.orderedHeaders,
      List(FunSimilarityEval.geneHeader),
      funSimilarityPerModel,
      "function-similarity"
    )

    val perturbedFixpointEvalPerModel = resultCombinations map { c =>
      InitialStatePerturbationEval.compareModelsForFixpointsFromPerturbedStates(
        hiddenModel, c, initialStates
      )
    }
    printPerModelTuples(
      InitialStatePerturbationEval.headers,
      List(InitialStatePerturbationEval.geneHeader),
      perturbedFixpointEvalPerModel,
      "stable-state-reachability-across-environments"
    )

    val perturbedReachabilityEvalPerModel = resultCombinations map {
      c =>
        InitialStatePerturbationEval
          .compareModelsForReachableStatesFromPerturbedStates(
            hiddenModel, c, initialStates)
    }
    printPerModelTuples(
      InitialStatePerturbationEval.headers,
      List(InitialStatePerturbationEval.geneHeader),
      perturbedReachabilityEvalPerModel,
      "all-state-reachability-across-environments"
    )

    val stateSpaceEvalTuples = resultCombinations map { c =>
      StateSpaceEval.compareStateSpaces(graphForSynthesis, c, initialStates)
    }
    TSVUtil.saveTupleMapsWithOrderedHeaders(
      StateSpaceEval.headers,
      stateSpaceEvalTuples,
      reporter.file("state-space-reproduction-eval.tsv"))

  }

  def printPerModelTuples(
    orderedHeaders: Seq[String],
    colsToExpand: Seq[String],
    rowsPerModel: Seq[Seq[Map[String, Any]]],
    prefix: String
  )(implicit reporter: Reporter): Unit = {
    for ((modelRows, i) <- rowsPerModel.zipWithIndex) {
      TSVUtil.saveTupleMapsWithOrderedHeaders(
        orderedHeaders,
        modelRows,
        reporter.file(s"$prefix-$i.tsv")
      )
    }

    val medianRows = TSVUtil.takeRowsMedian(colsToExpand, rowsPerModel.flatten)
    val minRows = TSVUtil.takeRowsMin(colsToExpand, rowsPerModel.flatten)
    val maxRows = TSVUtil.takeRowsMax(colsToExpand, rowsPerModel.flatten)

    TSVUtil.saveTupleMapsWithOrderedHeaders(
      orderedHeaders,
      medianRows,
      reporter.file(s"$prefix-median.tsv")
    )
    TSVUtil.saveTupleMapsWithOrderedHeaders(
      orderedHeaders,
      minRows,
      reporter.file(s"$prefix-min.tsv")
    )
    TSVUtil.saveTupleMapsWithOrderedHeaders(
      orderedHeaders,
      maxRows,
      reporter.file(s"$prefix-max.tsv")
    )
  }

}
