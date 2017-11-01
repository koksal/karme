package karme.evaluation.synthetic

import karme.ArgHandling
import karme.Opts
import karme.Reporter
import karme.evaluation.synthetic.examples.CAVModel
import karme.evaluation.synthetic.examples.CAVModelEvaluation
import karme.simulation.AsyncBooleanNetworkSimulation
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.SynthesisResult
import karme.synthesis.Synthesizer
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.util.TSVUtil
import karme.visualization.graph.StateGraphPlotter

object Workflow {

  def main(args: Array[String]): Unit = {
    implicit val opts = ArgHandling.parseOptions(args)
    implicit val reporter = new Reporter(opts.reporterOpts)

    run(
      hiddenModel = CAVModel.makePlosNetwork(),
      defaultInitialStates = Set(CAVModel.makeInitialState()),
      randomizedInitialStateInclusionRatio =
        opts.syntheticEvalOpts.randomizedInitialStateInclusionRatio,
      nodeDeletionRatio = opts.syntheticEvalOpts.nodeDeletionRatio,
      reconstructGraph = opts.syntheticEvalOpts.reconstructGraph,
      behaviorEvalFun = CAVModelEvaluation.evaluateModelBehavior
    )
  }

  def run(
    hiddenModel: Map[String, FunExpr],
    defaultInitialStates: Set[ConcreteBooleanState],
    randomizedInitialStateInclusionRatio: Option[Double],
    nodeDeletionRatio: Double,
    reconstructGraph: Boolean,
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
    val simulationGraph = AsyncBooleanNetworkSimulation
      .simulateOneStepWithStateGraph(hiddenModel, initialStates)
    val simulationStateToTimestamps = AsyncBooleanNetworkSimulation
      .simulateOneStepWithTimestamps(hiddenModel, initialStates)

    // (optionally) remove nodes per deletion ratio
    var graphForSynthesis = StateGraphPerturbation
      .deleteNodes(simulationGraph, nodeDeletionRatio)

    // (optionally) reconstruct graph per flag
    if (reconstructGraph) {
      graphForSynthesis = StateGraphReconstruction.reconstructStateGraph(
        simulationStateToTimestamps)
    }

    // logging graphs
    new StateGraphPlotter(reporter).plotDirectedGraph(graphForSynthesis,
      "graph-for-synthesis")

    // perform synthesis
    val synthesisResults = new Synthesizer(opts.synthOpts, reporter)
      .synthesizeForPositiveHardConstraints(graphForSynthesis)

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

  }

}
