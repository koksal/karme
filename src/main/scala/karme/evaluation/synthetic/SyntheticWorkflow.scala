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

  // TODO keep as an evaluation
  def evaluateTimestampOrientation(
    originalGraph: DirectedBooleanStateGraph,
    stateToTimestamps: Map[ConcreteBooleanState, Seq[Int]]
  ): Unit = {
    // check whether edge orientations agree with timestamp precedence
    var truePositiveEdges = Set[UnlabeledEdge[StateGraphVertex]]()
    var falsePositiveEdges = Set[UnlabeledEdge[StateGraphVertex]]()
    var ambiguousEdges = Set[UnlabeledEdge[StateGraphVertex]]()

    for (e <- originalGraph.E) {
      val ds = originalGraph.edgeDirections(e)
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
      s"${originalGraph.E.size}")

    new StateGraphPlotter(reporter).plotDirectedGraph(originalGraph,
      "simulated-state-graph")
  }

  // TODO to be kept with above
  def checkPrecedence(ts1: Seq[Int], ts2: Seq[Int]): Boolean = {
    ts1.min < ts2.min
    ts1.min <= ts2.min
    MathUtil.mean(ts1.map(_.toDouble)) <= MathUtil.mean(ts2.map(_.toDouble))
    ts1.exists(t1 => ts2.exists(t2 => t1 < t2))
    MathUtil.mean(ts1.map(_.toDouble)) < MathUtil.mean(ts2.map(_.toDouble))
  }

  // TODO to be added to main eval body
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
