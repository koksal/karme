package karme.evaluation.synthetic

import karme.Opts
import karme.Reporter
import karme.evaluation.PerturbationAnalysis
import karme.evaluation.synthetic.stategen.ExhaustiveStateEnumeration
import karme.graphs.Graphs.Forward
import karme.graphs.Graphs.UnlabeledEdge
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

object SyntheticWorkflow {

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

//      println(row.mkString("\t"))
    }

    println(s"Nb. consistent orientations: " +
      s"${truePositiveEdges.size}")
    println(s"Nb. consistent reverse orientations: " +
      s"${falsePositiveEdges.size}")
    println(s"Nb. ambiguous orientations: " +
      s"${ambiguousEdges.size}")
    println(s"Nb. total orientations: " +
      s"${originalGraph.E.size}")
  }

  // TODO to be kept with above
  def checkPrecedence(ts1: Seq[Int], ts2: Seq[Int]): Boolean = {
    ts1.min < ts2.min
    ts1.min <= ts2.min
    MathUtil.mean(ts1.map(_.toDouble)) <= MathUtil.mean(ts2.map(_.toDouble))
    ts1.exists(t1 => ts2.exists(t2 => t1 < t2))
    MathUtil.mean(ts1.map(_.toDouble)) < MathUtil.mean(ts2.map(_.toDouble))
  }

}

