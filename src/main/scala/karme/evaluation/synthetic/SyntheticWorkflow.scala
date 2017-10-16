package karme.evaluation.synthetic

import karme.Opts
import karme.Reporter
import karme.evaluation.PerturbationAnalysis
import karme.evaluation.synthetic.examples.CAVModel
import karme.evaluation.synthetic.fungen.RandomFunctionGeneration
import karme.evaluation.synthetic.stategen.ExhaustiveStateEnumeration
import karme.evaluation.synthetic.stategen.RandomStateGeneration
import karme.evaluation.synthetic.topology.RandomGraphGeneration
import karme.graphs.Graphs.Forward
import karme.graphs.Graphs.UnlabeledDiGraph
import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.graphs.StateGraphs.StateGraphVertex
import karme.printing.SynthesisResultLogger
import karme.simulation.AsyncBooleanNetworkSimulation
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.SynthesisResult
import karme.synthesis.Synthesizer
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.util.FileUtil
import karme.util.MathUtil
import karme.visualization.HistogramPlotInterface
import karme.visualization.graph.NetworkGraphPlotter
import karme.visualization.graph.StateGraphPlotter

class SyntheticWorkflow(opts: Opts, reporter: Reporter) {

  def synthesizeFromTimestampOrientations(): Unit ={
    // simulate model and produce graph + timestamps
    val labelToFun = CAVModel.makeNetwork()
    val initStates = Set(CAVModel.makeInitialState())

    val simulatedGraph = AsyncBooleanNetworkSimulation
      .simulateOneStepWithStateGraph(labelToFun, initStates)
    val stateToTimestamps = AsyncBooleanNetworkSimulation
      .simulateAnyStepsWithTimestamps(labelToFun, initStates).toMap

    // orient simulated graph with simulated timestamps
    val reorientedGraph = reorientGraphWithTimestamps(simulatedGraph,
      stateToTimestamps)

    // synthesize from oriented graph
    val results = new Synthesizer(opts.synthOpts, reporter)
      .synthesizeForPositiveHardConstraints(reorientedGraph)
    SynthesisResultLogger(results, reporter.file("functions.txt"))

    // check high-level property: reachability of stable states (with mutations)
    val resultCombinations = SynthesisResult.makeCombinations(results)
    for ((resultCombination, i) <- resultCombinations.zipWithIndex) yield {
      println(s"Testing inferred model $i:")
      evaluateModelBehavior(resultCombination)
    }
  }

  def evaluateModelBehavior(
    labelToFun: Map[String, FunExpr]
  ): Unit = {
    println(s"Wild-type fixpoint agreement: " +
      s"${modelHasCorrectWildTypeFixpoints(labelToFun)}")
    println(s"Number of disagreeing perturbations: " +
      s"${nbDisagreeingPerturbations(labelToFun)}")
  }

  def reorientGraphWithTimestamps(
    originalGraph: DirectedBooleanStateGraph,
    stateToTimestamps: Map[ConcreteBooleanState, Seq[Int]]
  ): DirectedBooleanStateGraph = {
    var newGraph = UnlabeledDiGraph[StateGraphVertex]()
    for (e <- originalGraph.E) {
      val ds = originalGraph.edgeDirections(e)
      assert(ds.size == 1)
      val d = ds.head

      val leftToRightPrecedence = checkPrecedence(
        stateToTimestamps(e.v1.state), stateToTimestamps(e.v2.state))
      val rightToLeftPrecedence = checkPrecedence(
        stateToTimestamps(e.v2.state), stateToTimestamps(e.v1.state))

      assert(!leftToRightPrecedence || !rightToLeftPrecedence)
      if (leftToRightPrecedence) {
        newGraph = newGraph.addEdge(e.v1, e.v2)
      }
      if (rightToLeftPrecedence) {
        newGraph = newGraph.addEdge(e.v2, e.v1)
      }
    }

    newGraph
  }

  def evaluateTimestampOrientation(): Unit = {
    // create simulation graph AND timestamps

    val labelToFun = CAVModel.makeNetwork()
    val initStates = Set(CAVModel.makeInitialState())

    val simulatedGraph = AsyncBooleanNetworkSimulation
      .simulateOneStepWithStateGraph(labelToFun, initStates)
    val stateToTimestamps = AsyncBooleanNetworkSimulation
      .simulateAnyStepsWithTimestamps(labelToFun, initStates).toMap

    // check whether edge orientations agree with timestamp precedence
    var nbConsistentOrientations = 0
    var nbConsistentReverseOrientations = 0

    for (e <- simulatedGraph.E) {
      val ds = simulatedGraph.edgeDirections(e)
      assert(ds.size == 1)
      val d = ds.head

      // get timestamps for each edge, compare with orientation
      val leftToRightPrecedence = checkPrecedence(
        stateToTimestamps(e.v1.state), stateToTimestamps(e.v2.state))
      val rightToLeftPrecedence = checkPrecedence(
        stateToTimestamps(e.v2.state), stateToTimestamps(e.v1.state))

      val timestampsConsistent = if (d == Forward) {
        leftToRightPrecedence
      } else {
        rightToLeftPrecedence
      }
      val timestampsConsistentReverse = if (d == Forward) {
        rightToLeftPrecedence
      } else {
        leftToRightPrecedence
      }

      if (timestampsConsistent) {
        nbConsistentOrientations +=1
      } else {
        if (timestampsConsistentReverse) {
          nbConsistentReverseOrientations +=1
        }
        println("Inconsistent edge:")
        println(s"${e.v1.id} - ${e.v2.id}, $d")
        println(stateToTimestamps(e.v1.state))
        println(stateToTimestamps(e.v2.state))
      }
    }

    println(s"Nb. consistent orientations: ${nbConsistentOrientations}")
    println(s"Nb. consistent reverse orientations: " +
      s"${nbConsistentReverseOrientations}")
    println(s"Nb. total orientations: ${simulatedGraph.E.size}")

    new StateGraphPlotter(reporter).plotDirectedGraph(simulatedGraph,
      "simulated-state-graph")
  }

  def checkPrecedence(ts1: Seq[Int], ts2: Seq[Int]): Boolean = {
    ts1.exists(t1 => ts2.exists(t2 => t1 < t2))
    ts1.min < ts2.min
    ts1.min <= ts2.min
    MathUtil.mean(ts1.map(_.toDouble)) <= MathUtil.mean(ts2.map(_.toDouble))
    MathUtil.mean(ts1.map(_.toDouble)) < MathUtil.mean(ts2.map(_.toDouble))
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

  def checkAlternativeModelStateSpaces(): Unit = {
    val initStates = Set(CAVModel.makeInitialState())
    val stateSets = CAVModel.makeSimplifiedNetworks() map { n =>
      AsyncBooleanNetworkSimulation
        .simulateOneStepWithStateGraph(n, initStates)
    }

    val allSetsAreEqual = stateSets.forall(
      set => stateSets.forall(
        otherSet => set == otherSet))

    if (allSetsAreEqual) {
      println("All simulated state sets are equal!")
    } else {
      println("Simulated state sets are not equal!")
    }

  }

  def checkAlternativeSemanticsStateSpaces(): Unit = {
    val labelToFun = CAVModel.makeNetwork()
    val initStates = Set(CAVModel.makeInitialState())

    val oneStepStates = AsyncBooleanNetworkSimulation
      .simulateOneStepWithTimestamps(labelToFun, initStates)
    val anyStepStates = AsyncBooleanNetworkSimulation
      .simulateAnyStepsWithTimestamps(labelToFun, initStates)

    if (oneStepStates != anyStepStates) {
      println("Timestamps are different.")
    } else {
      println("Timestamps are the same.")
    }

    if (oneStepStates.map(_._1) != anyStepStates.map(_._1)) {
      println("Reached states are different")
    } else {
      println("Reached states are the same.")
    }
  }

  def plotTimestamps(): Unit = {
    val labelToFun = CAVModel.makeNetwork()
    val initStates = Set(CAVModel.makeInitialState())

    val stateTimestampSetPairs = AsyncBooleanNetworkSimulation
      .simulateOneStepWithTimestamps(labelToFun, initStates)

    val sortedByMinTimestamp = stateTimestampSetPairs.toList.sortBy {
      case (state, tss) => tss.min
    }

    for (((state, tss), i) <- sortedByMinTimestamp.zipWithIndex) {
      println(s"State $i: ${tss.mkString(",")}")
    }
  }

  def testKnockouts(): Unit = {
    println("Testing original network...")
    nbDisagreeingPerturbations(CAVModel.makeNetwork())

    for (labelToFun <- CAVModel.makeSimplifiedNetworks()) {
      println("testing simplified network...")
      nbDisagreeingPerturbations(labelToFun)
    }
  }

  def modelHasCorrectWildTypeFixpoints(
    labelToFun: Map[String, FunExpr]
  ): Boolean = {
    val simulationFixpoints = findSimulationFixpoints(labelToFun,
      Set(CAVModel.makeInitialState()))
    simulationFixpoints == CAVModel.myeloidStableStates().values.toSet
  }

  def nbDisagreeingPerturbations(labelToFun: Map[String, FunExpr]): Int = {
    var nbDisagreeing = 0

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

      val fixpointCellTypeIds = simFixpointCellTypes.keySet

      println(s"Perturbing ${ke.knockoutVar}...")

      if (fixpointCellTypeIds == ke.observedOriginalAttractors) {
        println(s"Good! Reached ${fixpointCellTypeIds.mkString(",")}")
      } else {
        println(s"BAD!!! Reached ${fixpointCellTypeIds
          .mkString(",")}")
        nbDisagreeing += 1
      }

    }

    nbDisagreeing
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
    val simulatedStates = findSimulatedStates(labelToFun, initialStates)

    simulatedStates filter { s =>
      AsyncBooleanNetworkSimulation.stateIsFixpoint(labelToFun, s)
    }
  }

  def findSimulatedStates(
    labelToFun: Map[String, FunExpr],
    initialStates: Set[ConcreteBooleanState]
  ): Set[ConcreteBooleanState] = {
    val graphFromSimulation = AsyncBooleanNetworkSimulation
      .simulateOneStepWithStateGraph(labelToFun, initialStates)

    graphFromSimulation.V.map(_.state)
  }

  def runForModel(
    labelToFun: Map[String, FunExpr],
    initialStates: Set[ConcreteBooleanState],
    runReporter: Reporter
  ): Seq[Double] = {

    val graphFromSimulation = AsyncBooleanNetworkSimulation
      .simulateOneStepWithStateGraph(labelToFun, initialStates)

    new StateGraphPlotter(runReporter).plotDirectedGraph(graphFromSimulation,
      "simulated-state-graph")

    val results = new Synthesizer(opts.synthOpts, runReporter)
      .synthesizeForPositiveHardConstraints(graphFromSimulation)
    SynthesisResultLogger(results, runReporter.file("functions.txt"))

    val resultCombinations = SynthesisResult.makeCombinations(results)
    for ((resultCombination, i) <- resultCombinations.zipWithIndex) yield {
      new NetworkGraphPlotter(runReporter).plot(resultCombination,
        s"inferred-model-$i")
      val stateRecoveryMetric = computeStateRecovery(graphFromSimulation,
        resultCombination, initialStates)
      FileUtil.writeToFile(runReporter.file(s"state-recovery-metric-$i.txt"),
        stateRecoveryMetric.toString)
      stateRecoveryMetric
    }
  }

  private def computeStateRecovery(
    graphFromSimulation: DirectedBooleanStateGraph,
    recoveredFunctions: Map[String, FunExpr],
    initialStates: Set[ConcreteBooleanState]
  ) = {
    val recoveredGraph = AsyncBooleanNetworkSimulation
      .simulateOneStepWithStateGraph(recoveredFunctions, initialStates)

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
