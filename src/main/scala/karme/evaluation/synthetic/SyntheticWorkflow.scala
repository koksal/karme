package karme.evaluation.synthetic

import karme.Opts
import karme.Reporter
import karme.evaluation.PerturbationAnalysis
import karme.evaluation.synthetic.examples.CAVModel
import karme.evaluation.synthetic.examples.CAVModelEvaluation
import karme.evaluation.synthetic.fungen.RandomFunctionGeneration
import karme.evaluation.synthetic.stategen.ExhaustiveStateEnumeration
import karme.evaluation.synthetic.stategen.RandomStateGeneration
import karme.evaluation.synthetic.topology.RandomGraphGeneration
import karme.graphs.Graphs.Backward
import karme.graphs.Graphs.EdgeDirection
import karme.graphs.Graphs.Forward
import karme.graphs.Graphs.UnlabeledDiGraph
import karme.graphs.Graphs.UnlabeledEdge
import karme.graphs.StateGraphs.DirectedBooleanStateGraph
import karme.graphs.StateGraphs.StateGraphVertex
import karme.printing.SynthesisResultLogger
import karme.simulation.AsyncBooleanNetworkSimulation
import karme.synthesis.FunctionTrees.FunExpr
import karme.synthesis.SynthesisResult
import karme.synthesis.Synthesizer
import karme.synthesis.Transitions.ConcreteBooleanState
import karme.transformations.AverageComparisonTest
import karme.transformations.IncrementalStateGraphBuilder
import karme.transformations.MultiHammingEdgeExpansion
import karme.util.{CollectionUtil, FileUtil, MathUtil}
import karme.visualization.HistogramPlotInterface
import karme.visualization.graph.NetworkGraphPlotter
import karme.visualization.graph.StateGraphPlotter

import scala.util.Random

class SyntheticWorkflow(opts: Opts, reporter: Reporter) {

  val histogramPlotInterface = new HistogramPlotInterface()

  def extendedSimulation(): Unit = {
    // simulate from original state
    val statesInSimulation = AsyncBooleanNetworkSimulation.simulateOneStep(
      CAVModel.makePlosNetwork(),
      Set(CAVModel.makeInitialState())
    )
    println(s"Old graph size: ${statesInSimulation.size}")

    val allStates = new ExhaustiveStateEnumeration(
      CAVModel.makeInitialState().orderedKeys
    ).enumerateAllStates()

    // pick a state outside those observed
    val unobservedStates = allStates -- statesInSimulation
    val newInitialState = CollectionUtil.randomElement(unobservedStates)

    // simulate from both states
    val extendedStateGraph = AsyncBooleanNetworkSimulation
      .simulateOneStepWithStateGraph(
        CAVModel.makePlosNetwork(),
        // Set(CAVModel.makeInitialState(), newInitialState)
        allStates
      )
    println(s"New graph size: ${extendedStateGraph.V.size}")

    // synthesize from all constraints
    val results = new Synthesizer(opts.synthOpts, reporter)
      .synthesizeForPositiveHardConstraints(extendedStateGraph)
    SynthesisResultLogger(results, reporter.file("functions.txt"))

    val resultCombinations = SynthesisResult.makeCombinations(results)
    for ((resultCombination, i) <- resultCombinations.zipWithIndex) yield {
      println(s"Testing inferred model $i:")
      CAVModelEvaluation.evaluateModelBehavior(resultCombination)
    }
  }

  def synthesizeFromTimestampOrientations(): Unit ={
    // simulate model and produce graph + timestamps
    val labelToFun = CAVModel.makeNetwork()
    val initStates = Set(CAVModel.makeInitialState())

    val simulatedGraph = AsyncBooleanNetworkSimulation
      .simulateOneStepWithStateGraph(labelToFun, initStates)
    val stateToTimestamps = AsyncBooleanNetworkSimulation
      .simulateOneStepWithTimestamps(labelToFun, initStates).toMap

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
      CAVModelEvaluation.evaluateModelBehavior(resultCombination)
    }
  }

  def synthesizeWithGraphReconstruction(): Unit = {
    // simulate model and produce graph + timestamps
    val labelToFun = CAVModel.makeNetwork()
    val initStates = Set(CAVModel.makeInitialState())

    val simulatedGraph = AsyncBooleanNetworkSimulation
      .simulateOneStepWithStateGraph(labelToFun, initStates)
    val stateToTimestamps = AsyncBooleanNetworkSimulation
      .simulateOneStepWithTimestamps(labelToFun, initStates)

    val (exp, traj) = SimulationToExperiment.makeExperimentAndTrajectory(
      stateToTimestamps)

    val graphBuilder = new IncrementalStateGraphBuilder(exp, Seq(traj),
      new AverageComparisonTest)
    var graph = graphBuilder.buildGraph

//    new StateGraphPlotter(reporter).plotDirectedGraph(graph,
//      "reconstructed-non-expanded-graph")

    // expandMultiHammingEdges
    graph = new MultiHammingEdgeExpansion(graph).expandMultiHammingEdges()

//    println(graph.E.count(e => graph.edgeDirections(e).size > 1))
//    new StateGraphPlotter(reporter).plotDirectedGraph(graph,
//      "reconstructed-expanded-graph")

    diffGraphs(simulatedGraph, graph)
    sys.exit(0)

    val results = new Synthesizer(opts.synthOpts, reporter)
      .synthesizeForPositiveHardConstraints(simulatedGraph)
    SynthesisResultLogger(results, reporter.file("functions.txt"))

    // check high-level property: reachability of stable states (with mutations)
    val resultCombinations = SynthesisResult.makeCombinations(results)
    for ((resultCombination, i) <- resultCombinations.zipWithIndex) yield {
      println(s"Testing inferred model $i:")
      CAVModelEvaluation.evaluateModelBehavior(resultCombination)
    }
  }

  def diffGraphs(
    g1: DirectedBooleanStateGraph, g2: DirectedBooleanStateGraph
  ) = {
    def reverse(d: EdgeDirection): EdgeDirection = d match {
      case Forward => Backward
      case Backward => Forward
    }

    val states1 = g1.V.map(_.state)
    val states2 = g2.V.map(_.state)

    println(s"States only in 1: ${(states1 -- states2).size}")
    println(s"States only in 2: ${(states2 -- states1).size}")

    var nbOrigDirectionCaptured = 0
    var nbOrigDirectionNonCaptured = 0
    var nbMissedEdges = 0

    for (e1 <- g1.E) {
      g2.E.find { e2 =>
        Set(e1.v1.state, e1.v2.state) == Set(e2.v1.state, e2.v2.state)
      } match {
        case Some(e2) => {
          val ds1 = g1.edgeDirections(e1)
          val ds2 = g2.edgeDirections(e2)
          val sameEdgeOrder = e1.v1.state == e2.v1.state
          if (sameEdgeOrder) {
            if (ds1.subsetOf(ds2)) {
              nbOrigDirectionCaptured += 1
            } else {
              nbOrigDirectionNonCaptured += 1
            }
          } else {
            if (ds1.subsetOf(ds2.map(reverse))) {
              nbOrigDirectionCaptured += 1
            } else {
              nbOrigDirectionNonCaptured += 1
            }
          }
        }
        case None => {
          nbMissedEdges += 1
        }
      }
    }

    println(s"Directions captured: $nbOrigDirectionCaptured")
    println(s"Directions non-captured: $nbOrigDirectionNonCaptured")
    println(s"Missed edges: $nbMissedEdges")

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
      .simulateOneStepWithTimestamps(labelToFun, initStates).toMap

    // check whether edge orientations agree with timestamp precedence
    var truePositiveEdges = Set[UnlabeledEdge[StateGraphVertex]]()
    var falsePositiveEdges = Set[UnlabeledEdge[StateGraphVertex]]()
    var ambiguousEdges = Set[UnlabeledEdge[StateGraphVertex]]()

    for (e <- simulatedGraph.E) {
      val ds = simulatedGraph.edgeDirections(e)
      assert(ds.size == 1)
      val d = ds.head

      // get timestamps for each edge, compare with orientation
      val leftToRightPrecedence = checkPrecedence(
        stateToTimestamps(e.v1.state), stateToTimestamps(e.v2.state))
      val rightToLeftPrecedence = checkPrecedence(
        stateToTimestamps(e.v2.state), stateToTimestamps(e.v1.state))

      val (forwardConsistent, backwardConsistent) = if (d == Forward) {
        (leftToRightPrecedence, rightToLeftPrecedence)
      } else {
        (rightToLeftPrecedence, leftToRightPrecedence)
      }

      if (forwardConsistent && !backwardConsistent) {
        truePositiveEdges += e
      }
      if (backwardConsistent && !forwardConsistent) {
        falsePositiveEdges += e
      }
      if (forwardConsistent && backwardConsistent) {
        ambiguousEdges += e
      }

      val row = List(
        e.v1.id,
        e.v2.id,
        d,
        forwardConsistent,
        backwardConsistent,
        stateToTimestamps(e.v1.state).mkString(", "),
        stateToTimestamps(e.v2.state).mkString(", ")
      )

      println(row.mkString("\t"))
    }

    println(s"Nb. consistent orientations: " +
      s"${truePositiveEdges.size}")
    println(s"Nb. consistent reverse orientations: " +
      s"${falsePositiveEdges.size}")
    println(s"Nb. ambiguous orientations: " +
      s"${ambiguousEdges.size}")
    println(s"Nb. total orientations: " +
      s"${simulatedGraph.E.size}")

    new StateGraphPlotter(reporter).plotDirectedGraph(simulatedGraph,
      "simulated-state-graph")
  }



  def checkPrecedence(ts1: Seq[Int], ts2: Seq[Int]): Boolean = {
    ts1.min < ts2.min
    ts1.min <= ts2.min
    MathUtil.mean(ts1.map(_.toDouble)) <= MathUtil.mean(ts2.map(_.toDouble))
    ts1.exists(t1 => ts2.exists(t2 => t1 < t2))
    MathUtil.mean(ts1.map(_.toDouble)) < MathUtil.mean(ts2.map(_.toDouble))
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

    val oneStepStateSet = oneStepStates.map(_._1)
    val anyStepStateSet = anyStepStates.map(_._1)

    if (oneStepStateSet != anyStepStateSet) {
      println("Reached states are different")
      println(s"Nb. one-step states: ${oneStepStates.size}")
      println(s"Nb. any-step states: ${anyStepStates.size}")
      println(s"One-step is subset: " +
        s"${oneStepStateSet.subsetOf(anyStepStateSet)}")
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

object SyntheticWorkflow {

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

}
