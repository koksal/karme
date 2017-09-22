package karme.evaluation.synthetic

import karme.Opts
import karme.Reporter
import karme.evaluation.synthetic.fungen.RandomFunctionGeneration
import karme.evaluation.synthetic.stategen.RandomStateGeneration
import karme.evaluation.synthetic.topology.LinearNetworkGeneration
import karme.simulation.AsyncBooleanNetworkSimulation
import karme.transformations.DistributionComparisonTest
import karme.transformations.IncrementalStateGraphBuilder

class SyntheticWorkflow(opts: Opts, reporter: Reporter) {

  def run(): Unit = {
    // 1. Create network topology
    val topology = new LinearNetworkGeneration(10).generate()

    // 2. Create functions for each node
    val labelToFun = new RandomFunctionGeneration().generate(topology)

    // 3. Pick an initial state
    val initialStates = new RandomStateGeneration(labelToFun.keySet)
      .generateInitialStates()

    // 4. Simulate network
    val stateTimestampPairs = AsyncBooleanNetworkSimulation
      .simulateWithTimestamps(labelToFun, initialStates)

    // 5. Create graph from simulation results
    // 5a. Make experiment and trajectory from simulation results
    val (experiment, trajectory) = SimulationToExperiment
      .makeExperimentAndTrajectory(stateTimestampPairs)

    // 5b. Make graph from synthetic experiment
    val graphBuilder = new IncrementalStateGraphBuilder(experiment,
      Seq(trajectory), DistributionComparisonTest.fromOptions(
        opts.inputTransformerOpts.distributionComparisonMethod))
    val graph = graphBuilder.buildGraph
    val graphSources = graphBuilder.initialNodes(graph)

    // TODO Optionally alter simulated data (sample, flip bits)

    // 6. Run inference

    // 7. Compare synthesized functions against original functions
    //    (visualize inferred GRN)
  }

}
