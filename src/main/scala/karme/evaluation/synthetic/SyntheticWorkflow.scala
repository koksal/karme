package karme.evaluation.synthetic

import karme.Opts
import karme.Reporter
import karme.evaluation.PerturbationAnalysis
import karme.evaluation.synthetic.examples.CAVModel
import karme.evaluation.synthetic.fungen.RandomFunctionGeneration
import karme.evaluation.synthetic.stategen.ExhaustiveStateEnumeration
import karme.evaluation.synthetic.stategen.RandomStateGeneration
import karme.evaluation.synthetic.topology.RandomGraphGeneration
import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.printing.SynthesisResultLogger
import karme.simulation.AsyncBooleanNetworkSimulation
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.SynthesisResult
import karme.synthesis.Synthesizer
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.util.FileUtil
import karme.visualization.HistogramPlotInterface
import karme.visualization.graph.NetworkGraphPlotter
import karme.visualization.graph.StateGraphPlotter

class SyntheticWorkflow(opts: Opts, reporter: Reporter) {

  def evaluateTimestampOrientation(): Unit = {
    // create simulation graph AND timestamps

    val labelToFun = CAVModel.makeNetwork()
    val initStates = Set(CAVModel.makeInitialState())

    val simulatedGraph = AsyncBooleanNetworkSimulation
      .simulateWithStateGraph(labelToFun, initStates)
    val simulatedStateTimestamps = AsyncBooleanNetworkSimulation
      .simulateWithTimestamps(labelToFun, initStates)

    // check whether edge orientations agree with timestamp precedence
    for (e <- simulatedGraph.E) {
      val ds = simulatedGraph.edgeDirections(e)
      assert(ds.size == 1)
      val d = ds.head

      // get timestamps for each edge, compare with orientation
      
    }

  }

  def runHandCuratedModel(): Unit = {
    val labelToFun = CAVModel.makeNetwork()
    val initStates = Set(CAVModel.makeInitialState())

    runForModel(labelToFun, initStates, reporter)

    for (inferredFuns <- CAVModel.makeSimplifiedNetworks()) {
      println("Running inferred functions...")
      runForModel(inferredFuns, initStates, reporter)
    }
  }

  def plotTimestamps(): Unit = {
    val labelToFun = CAVModel.makeNetwork()
    val initStates = Set(CAVModel.makeInitialState())

    val stateTimestampSetPairs = AsyncBooleanNetworkSimulation
      .simulateWithTimestamps(labelToFun, initStates)

    val sortedByMinTimestamp = stateTimestampSetPairs.toList.sortBy {
      case (state, tss) => tss.min
    }

    for (((state, tss), i) <- sortedByMinTimestamp.zipWithIndex) {
      println(s"State $i: ${tss.mkString(",")}")
    }
  }

  def testKnockouts(): Unit = {
    println("Testing original network...")
    testModelForPerturbations(CAVModel.makeNetwork())

    for (labelToFun <- CAVModel.makeSimplifiedNetworks()) {
      println("testing simplified network...")
      testModelForPerturbations(labelToFun)
    }
  }

  def testModelForPerturbations(labelToFun: Map[String, FunExpr]): Unit = {
    for (ke <- CAVModel.knockoutExperiments()) {
      val perturbedFuns = PerturbationAnalysis.knockoutVariable(labelToFun,
        ke.knockoutVar)

      val perturbedInitialState = CAVModel.makeInitialState().replaceValue(
        ke.knockoutVar, false)

      val simulationFixpoints = findSimulationFixpoints(perturbedFuns,
        Set(perturbedInitialState))

      val simFixpointCellTypes = CAVModel.myeloidStableStates() filter {
        case (id, state) => simulationFixpoints.contains(state)
      }

      val nonOriginalSimulationFixpoints =
        simulationFixpoints.size - simFixpointCellTypes.size

      val fixpointCellTypeIds = simFixpointCellTypes.keySet

      println(s"Perturbing ${ke.knockoutVar}...")

      if (fixpointCellTypeIds == ke.observedOriginalAttractors) {
        println(s"Good! Reached ${fixpointCellTypeIds.mkString(",")}")
      } else {
        println(s"BAD!!! Reached ${fixpointCellTypeIds
          .mkString(",")}")
      }

      if (nonOriginalSimulationFixpoints == ke.nbNewAttractors) {
        println(s"Good! Reached ${nonOriginalSimulationFixpoints} new fixpoints.")
      } else {
        println(s"BAD!!! Reached ${nonOriginalSimulationFixpoints} new fixpoints.")
      }
    }

    println()
  }

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

    var recoveryRatios = Seq[Double]()
    for ((initialStates, runIndex) <- initialStateSets.zipWithIndex) {
      val runReporter = reporter.subfolderReporter(s"run-$runIndex")

      FileUtil.writeToFile(runReporter.file("initial-state.txt"),
        initialStates.mkString("\n"))

      recoveryRatios ++= runForModel(labelToFun, initialStates, runReporter)
    }

    new HistogramPlotInterface().plot(recoveryRatios,
      reporter.file("recovery-ratios.pdf"))
  }

  def findAllFixpoints(
    labelToFun: Map[String, FunExpr]
  ): Set[ConcreteBooleanState] = {
    val allStateSingletons = new ExhaustiveStateEnumeration(
      labelToFun.keySet.toList).enumerateInitialStates()

    val fixpointSingletons = allStateSingletons filter { s =>
      assert(s.size == 1)
      AsyncBooleanNetworkSimulation.stateIsFixpoint(labelToFun, s.head)
    }

    fixpointSingletons.map(_.head).toSet
  }

  def runForPerturbationsFromFixpoints(): Unit = {
    val topology = makeTopology()

    val labelToFun = new RandomFunctionGeneration().generate(topology)
    FileUtil.writeToFile(reporter.file("model-functions.txt"),
      labelToFun.mkString("\n"))
    new NetworkGraphPlotter(reporter).plot(labelToFun, "hidden-model")

    val allStates = new ExhaustiveStateEnumeration(labelToFun.keySet.toList)
      .enumerateAllStates()

    val fixpoints = allStates filter { s =>
      AsyncBooleanNetworkSimulation.stateIsFixpoint(labelToFun, s)
    }

    println(s"There are ${fixpoints.size} fixpoint states.")
    FileUtil.writeToFile(reporter.file("fixpoint-states.txt"),
      fixpoints.mkString("\n"))

    for ((fixpoint, i) <- fixpoints.zipWithIndex) {
      perturbVariables(labelToFun, fixpoint,
        reporter.subfolderReporter(s"fixpoint-$i"))
    }
  }

  def perturbVariables(
    labelToFun: Map[String, FunExpr],
    state: ConcreteBooleanState,
    runReporter: Reporter
  ) = {
    for (label <- labelToFun.keySet) {
      val overriddenFunctions = PerturbationAnalysis
        .overrideWithConstantFunction(labelToFun, label, !state.value(label))

      val recoveryRatio = runForModel(overriddenFunctions, Set(state),
        runReporter.subfolderReporter(s"perturb-$label"))
    }
  }

  def findSimulationFixpoints(
    labelToFun: Map[String, FunExpr],
    initialStates: Set[ConcreteBooleanState]
  ): Set[ConcreteBooleanState] = {
    val graphFromSimulation = AsyncBooleanNetworkSimulation
      .simulateWithStateGraph(labelToFun, initialStates)

    val simulatedStates = graphFromSimulation.V.map(_.state)
    println(s"Simulated states: ${simulatedStates.size}")

    simulatedStates filter { s =>
      AsyncBooleanNetworkSimulation.stateIsFixpoint(labelToFun, s)
    }
  }

  def runForModel(
    labelToFun: Map[String, FunExpr],
    initialStates: Set[ConcreteBooleanState],
    runReporter: Reporter
  ): Seq[Double] = {

    val graphFromSimulation = AsyncBooleanNetworkSimulation
      .simulateWithStateGraph(labelToFun, initialStates)
    println(s"# graph nodes: ${graphFromSimulation.V.size}")

    Nil
//    new StateGraphPlotter(runReporter).plotDirectedGraph(graphFromSimulation,
//      "simulated-state-graph")
//
//    val simulatedStates = graphFromSimulation.V.map(_.state)
//    val fixpointsInSimulatedStates = simulatedStates filter { s =>
//      AsyncBooleanNetworkSimulation.stateIsFixpoint(labelToFun, s)
//    }
//    println(s"There are ${fixpointsInSimulatedStates.size} simulated fixpoint" +
//      s" states.")
//    FileUtil.writeToFile(reporter.file("fixpoint-states-in-simulation.txt"),
//      fixpointsInSimulatedStates.mkString("\n"))
//
//    val results = new Synthesizer(opts.synthOpts, runReporter)
//      .synthesizeForPositiveHardConstraints(graphFromSimulation)
//    SynthesisResultLogger(results, runReporter.file("functions.txt"))
//
//    val resultCombinations = SynthesisResult.makeCombinations(results)
//    for ((resultCombination, i) <- resultCombinations.zipWithIndex) yield {
//      new NetworkGraphPlotter(runReporter).plot(resultCombination,
//        s"inferred-model-$i")
//      val stateRecoveryMetric = computeStateRecovery(graphFromSimulation,
//        resultCombination, initialStates)
//      FileUtil.writeToFile(runReporter.file(s"state-recovery-metric-$i.txt"),
//        stateRecoveryMetric.toString)
//      stateRecoveryMetric
//    }
  }

  private def computeStateRecovery(
    graphFromSimulation: DirectedBooleanStateGraph,
    recoveredFunctions: Map[String, FunExpr],
    initialStates: Set[ConcreteBooleanState]
  ) = {
    val recoveredGraph = AsyncBooleanNetworkSimulation
      .simulateWithStateGraph(recoveredFunctions, initialStates)

    val originalStates = graphFromSimulation.V.map(_.state)
    val recoveredStates = recoveredGraph.V.map(_.state)

    compareStates(originalStates, recoveredStates)
  }

  private def compareStates(
    originalStates: Set[ConcreteBooleanState],
    recoveredStates: Set[ConcreteBooleanState]
  ): Double = {
    val missedStates = originalStates -- recoveredStates
    val unobservedStates = recoveredStates -- originalStates

    missedStates.size + unobservedStates.size
  }

  private def computeRecoveryRatio(
    original: Map[String, FunExpr],
    predicted: Map[String, FunExpr]
  ): Double = {
    var misses = 0
    var successes = 0
    var failures = 0
    for ((label, originalFunction) <- original) {
      predicted.get(label) match {
        case None => {
          misses += 1
        }
        case Some(predictedFunction) => {
          if (predictedFunction == originalFunction) {
            successes += 1
          } else {
            failures += 1
          }
        }
      }
    }

    successes.toDouble / original.size
  }

  private def makeTopology() = {
    // new BranchingNetworkGeneration(2).generate()
    // new DAGGeneration(2).generate()
    // new CyclicNetworkGeneration(5).generate()
    new RandomGraphGeneration(8).generate()
  }

}
