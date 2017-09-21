package karme

import karme.CellTrajectories.CellTrajectory
import karme.Experiments.Experiment
import karme.evaluation.{ClusterPairExpansion, FunctionIOPairs, PerturbationAnalysis}
import karme.graphs.StateGraphs.{DirectedBooleanStateGraph, StateGraphVertex}
import karme.parsing.NamesParser
import karme.printing.{IOPairLogger, SynthesisResultLogger}
import karme.store.ClusteringStore
import karme.synthesis.{SynthesisResult, Synthesizer}
import karme.transformations.{DistributionComparisonTest, InputTransformer, LinearGraphDerivativeAnalysis, TransformResult}
import karme.visualization.StateGraphPlotter

object Main {

  def main(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)
    val reporter = new Reporter(opts.reporterOpts)

    val annotationContext = AnnotationContext.fromOpts(opts.annotationOpts)
    val inputContext = InputContext.fromOpts(opts.inputFileOpts)

    runLinearInference(opts, reporter, annotationContext,
      inputContext.rawExperiment, inputContext.trajectories)
  }

  def runLinearInference(
    opts: Opts,
    reporter: Reporter,
    annotationContext: AnnotationContext,
    rawExperiment: Experiment[Double],
    trajectories: Seq[CellTrajectory]
  ) = {
    val inputTransformer = new InputTransformer(
      rawExperiment,
      trajectories,
      opts.inputTransformerOpts,
      annotationContext
    )(reporter)

    val experiment = inputTransformer.getTransformedContinuousExperiment()
    val kdExperiment = PredictionLibrary.aggregate(
      InputContext.getKnockdownExperiments(opts.inputFileOpts))

//    new LinearGraphAnalysis(reporter).analyze(experiment,
//      trajectories.head, kdExperiment)
    new LinearGraphDerivativeAnalysis(
      DistributionComparisonTest.fromOptions(
        opts.inputTransformerOpts.distributionComparisonMethod),
      opts.inputTransformerOpts.distributionComparisonPValue)(reporter)
      .analyze(experiment, trajectories.head, kdExperiment)
  }

  def runInference(
    opts: Opts,
    reporter: Reporter,
    annotationContext: AnnotationContext,
    rawExperiment: Experiment[Double],
    trajectories: Seq[CellTrajectory]
  ): Seq[(String, String)] = {
    val inputTransformer = new InputTransformer(
      rawExperiment,
      trajectories,
      opts.inputTransformerOpts,
      annotationContext
    )(reporter)

    val TransformResult(graph, sources, clustering) =
      inputTransformer.transform()

    logGraph(graph, sources, annotationContext, reporter)

    if (opts.inputTransformerOpts.clusteringOpts.cluster) {
      new ClusteringStore(opts.reporterOpts.outFolder).store(
        clustering.get.clusterToMembers)
    }

    val results = runSynthesis(opts, graph, reporter)

    getIOPairs(results, clustering, reporter)
  }

  def runSynthesis(
    opts: Opts,
    directedStateGraph: DirectedBooleanStateGraph,
    reporter: Reporter
  ): Map[String, Set[SynthesisResult]] = {
    val synthesizer = new Synthesizer(opts.synthOpts, reporter)

    val results = synthesizer.synthesizeForPositiveHardConstraints(
      directedStateGraph)

    SynthesisResultLogger(results, reporter.file("functions.txt"))
    results
  }

  def runPerturbationEval(
    results: Map[String, Set[SynthesisResult]],
    sources: Set[StateGraphVertex],
    graph: DirectedBooleanStateGraph,
    clustering: Clustering,
    opts: Opts,
    reporter: Reporter
  ): Unit = {
    val functionsToEvaluate = results collect {
      case (name, results) if results.nonEmpty => {
        name -> results.head.functions.head
      }
    }
    val initialStates = sources.map(_.state)
    val targets = NamesParser.parseNames(
      opts.evalOpts.perturbationTargetsFile.get)
    val expectedDrivers = NamesParser.parseNames(
      opts.evalOpts.expectedDriversFile.get)
    val perturbationAnalysis = new PerturbationAnalysis(
      functionsToEvaluate,
      graph,
      initialStates,
      clustering,
      targets,
      expectedDrivers
    )(reporter)
    perturbationAnalysis.findGeneDrivers()
  }

  def getIOPairs(
    results: Map[String, Set[SynthesisResult]],
    clustering: Option[Clustering],
    reporter: Reporter
  ): Seq[(String, String)] = {
    var nonMappedPairs = Seq[(String, String)]()
    for ((label, labelResults) <- results) {
      for (res <- labelResults) {
        nonMappedPairs ++= FunctionIOPairs.funInputOutputPairs(label, res)
      }
    }

    IOPairLogger.logPairs(nonMappedPairs,
      reporter.file("non-mapped-io-pairs.csv"))

    clustering match {
      case Some(cs) => {
        val mappedPairs = new ClusterPairExpansion(
          cs.clusterToMembers).clusterMemberPairs(nonMappedPairs)

        IOPairLogger.logPairs(mappedPairs, reporter.file("mapped-io-pairs.csv"))
        mappedPairs
      }
      case None => {
        nonMappedPairs
      }
    }
  }

  def logGraph(
    graph: DirectedBooleanStateGraph,
    sources: Set[StateGraphVertex],
    annotationContext: AnnotationContext,
    reporter: Reporter
  ): Unit = {
    new StateGraphPlotter(reporter).plotDirectedGraph(
      graph,
      "state-graph",
      cellClustering = annotationContext.cellClustering,
      nodeHighlightGroups = List(sources.map(_.state))
    )
  }
}
