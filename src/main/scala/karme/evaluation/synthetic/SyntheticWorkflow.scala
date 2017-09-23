package karme.evaluation.synthetic

import karme.Opts
import karme.Reporter
import karme.evaluation.synthetic.fungen.RandomFunctionGeneration
import karme.evaluation.synthetic.stategen.RandomStateGeneration
import karme.evaluation.synthetic.topology.LinearNetworkGeneration
import karme.printing.ExperimentLogger
import karme.printing.SynthesisResultLogger
import karme.simulation.AsyncBooleanNetworkSimulation
import karme.synthesis.Synthesizer
import karme.transformations.DistributionComparisonTest
import karme.transformations.IncrementalStateGraphBuilder
import karme.util.FileUtil
import karme.visualization.graph.NetworkGraphPlotter
import karme.visualization.graph.StateGraphPlotter

class SyntheticWorkflow(opts: Opts, reporter: Reporter) {

  def run(): Unit = {
    // 1. Create network topology
    val topology = new LinearNetworkGeneration(2).generate()

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

    // 4. Simulate network
    val stateTimestampPairs = AsyncBooleanNetworkSimulation
      .simulateWithTimestamps(labelToFun, initialStates)
    FileUtil.writeToFile(reporter.file("simulated-states.txt"),
      stateTimestampPairs.mkString("\n"))

    // TODO plot simulation?

    // 5. Create graph from simulation results
    // 5a. Make experiment and trajectory from simulation results
    val (experiment, trajectory) = SimulationToExperiment
      .makeExperimentAndTrajectory(stateTimestampPairs)
    ExperimentLogger.saveToFile(experiment,
      reporter.file("simulated-experiment.csv"))

    // 5b. Make graph from synthetic experiment
    val graphBuilder = new IncrementalStateGraphBuilder(experiment,
      Seq(trajectory), DistributionComparisonTest.fromOptions(
        opts.inputTransformerOpts.distributionComparisonMethod))
    val graph = graphBuilder.buildGraph
    new StateGraphPlotter(reporter).plotDirectedGraph(graph,
      "reconstructed-state-graph")

    // TODO alternatively, recover graph directly from simulation

    // TODO log/visualize reconstructed graph

    // TODO Optionally alter simulated data (sample, flip bits)

    // 6. Run inference
    val results = new Synthesizer(opts.synthOpts, reporter)
      .synthesizeForPositiveHardConstraints(graph)
    SynthesisResultLogger(results, reporter.file("functions.txt"))

    // 7. Compare synthesized functions against original functions
    //    (visualize inferred GRN)
  }

}
