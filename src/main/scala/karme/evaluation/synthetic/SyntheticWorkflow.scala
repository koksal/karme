package karme.evaluation.synthetic

import karme.Opts
import karme.Reporter
import karme.evaluation.synthetic.fungen.RandomFunctionGeneration
import karme.evaluation.synthetic.stategen.ExhaustiveStateEnumeration
import karme.evaluation.synthetic.stategen.RandomStateGeneration
import karme.evaluation.synthetic.topology.BranchingNetworkGeneration
import karme.evaluation.synthetic.topology.LinearNetworkGeneration
import karme.printing.ExperimentLogger
import karme.printing.SynthesisResultLogger
import karme.simulation.AsyncBooleanNetworkSimulation
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.SynthesisResult
import karme.synthesis.Synthesizer
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.transformations.DistributionComparisonTest
import karme.transformations.IncrementalStateGraphBuilder
import karme.util.FileUtil
import karme.util.MathUtil
import karme.visualization.graph.NetworkGraphPlotter
import karme.visualization.graph.StateGraphPlotter

class SyntheticWorkflow(opts: Opts, reporter: Reporter) {

  def run(): Unit = {
    // 1. Create network topology
    val topology = makeTopology()

    // 2. Create functions for each node
    val labelToFun = new RandomFunctionGeneration().generate(topology)
    FileUtil.writeToFile(reporter.file("model-functions.txt"),
      labelToFun.mkString("\n"))
    new NetworkGraphPlotter(reporter).plot(labelToFun, "hidden-model")

    // 3. Pick an initial state
    val initialStates = new RandomStateGeneration(labelToFun.keySet)
      .generateInitialStates()
    FileUtil.writeToFile(reporter.file("initial-state.txt"),
      initialStates.mkString("\n"))

    runForModel(labelToFun, initialStates, reporter)
  }

  def runMany(): Unit = {
    // 1. Create network topology
    val topology = makeTopology()

    // 2. Create functions for each node
    val labelToFun = new RandomFunctionGeneration().generate(topology)
    FileUtil.writeToFile(reporter.file("model-functions.txt"),
      labelToFun.mkString("\n"))
    new NetworkGraphPlotter(reporter).plot(labelToFun, "hidden-model")

    // 3. Pick an initial state
    val initialStateSets = new ExhaustiveStateEnumeration(
      labelToFun.keySet.toList).enumerateInitialStates()

    for ((initialStates, runIndex) <- initialStateSets.zipWithIndex) {
      val runReporter = reporter.subfolderReporter(s"run-$runIndex")

      FileUtil.writeToFile(runReporter.file("initial-state.txt"),
        initialStates.mkString("\n"))

      runForModel(labelToFun, initialStates, runReporter)
    }
  }

  def runForModel(
    labelToFun: Map[String, FunExpr],
    initialStates: Set[ConcreteBooleanState],
    runReporter: Reporter
  ): Unit = {

    // 4a. Simulate graph and directly create a state graph
    val graphFromSimulation = AsyncBooleanNetworkSimulation
      .simulateWithStateGraph(labelToFun, initialStates)
    new StateGraphPlotter(runReporter).plotDirectedGraph(graphFromSimulation,
      "simulated-state-graph")

    // 4b. Simulate network and create states with timestamps
    val stateTimestampPairs = AsyncBooleanNetworkSimulation
      .simulateWithTimestamps(labelToFun, initialStates)
    FileUtil.writeToFile(runReporter.file("simulated-states.txt"),
      stateTimestampPairs.mkString("\n"))

    // 5. Recover graph from simulated states and timestamps
    // 5a. Make experiment and trajectory from simulation results
    val (experiment, trajectory) = SimulationToExperiment
      .makeExperimentAndTrajectory(stateTimestampPairs)
    ExperimentLogger.saveToFile(experiment,
      runReporter.file("simulated-experiment.csv"))

    // 5b. Make graph from synthetic experiment
    val graphBuilder = new IncrementalStateGraphBuilder(experiment,
      Seq(trajectory), DistributionComparisonTest.fromOptions(
        opts.inputTransformerOpts.distributionComparisonMethod))
    val graph = graphBuilder.buildGraph
    new StateGraphPlotter(runReporter).plotDirectedGraph(graph,
      "reconstructed-state-graph")

    // TODO Optionally alter simulated data (sample, flip bits)

    // 6. Run inference
    val results = new Synthesizer(opts.synthOpts, runReporter)
      .synthesizeForPositiveHardConstraints(graphFromSimulation)
    SynthesisResultLogger(results, runReporter.file("functions.txt"))

    // 7. Compare synthesized functions against original functions
    //    (visualize inferred GRN)
    val resultCombinations = makeResultCombinations(results)
    for ((resultCombination, i) <- resultCombinations.zipWithIndex) {
      new NetworkGraphPlotter(runReporter).plot(resultCombination,
        s"inferred-model-$i")
    }
  }

  private def makeTopology() = {
    new BranchingNetworkGeneration(2).generate()
  }

  private def makeResultCombinations(
    labelToResults: Map[String, Set[SynthesisResult]]
  ): Seq[Map[String, FunExpr]] = {
    val labels = labelToResults.keySet.toList
    val setsForProduct = labels map { label =>
      labelToResults(label).flatMap(_.functions)
    }

    val product = MathUtil.cartesianProduct(setsForProduct)

    product.toList map { functionCombination =>
      labels.zip(functionCombination).toMap
    }
  }
}
