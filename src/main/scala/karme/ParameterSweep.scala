package karme

import java.io.File

import karme.graphs.Graphs.UnlabeledEdge
import karme.graphs.StateGraphs.{DirectedBooleanStateGraph, StateGraphVertex, UndirectedStateGraphOps}
import karme.printing.SynthesisResultLogger
import karme.synthesis.{SynthesisResult, Synthesizer}
import karme.transformations.InputTransformer
import karme.util.{CollectionUtil, FileUtil, ParUtil, TimingUtil}

object ParameterSweep {

  def main(args: Array[String]): Unit = {
    val opts = ArgHandling.parseOptions(args)

    val inputTransformerOptsRange = expandOpts(opts.inputTransformerOpts,
      OptParameterRangeExpander.RANGE_EXPANDERS)
    println(s"Expanded to ${inputTransformerOptsRange.size} options.")

    synthesizeForRange(opts, inputTransformerOptsRange)
  }

  def synthesizeForRange[U](
    baseOpts: Opts,
    inputTransOptRange: Seq[InputTransformerOpts]
  ): Seq[(Map[String, Set[String]], Map[String, Set[SynthesisResult]])] = {
    val annotCtx = AnnotationContext.fromOpts(baseOpts.annotationOpts)

    val clustGraphPairs =
      ParUtil.withParallelism(8, inputTransOptRange).zipWithIndex.flatMap {
        case (opt, i) => {
          val runFolder = new File(baseOpts.reporterOpts.outFolder,
            s"graph-creation-$i")
          val runReporter = new Reporter(baseOpts.reporterOpts.copy(
            outFolder = runFolder))

          // TODO: use runReporter
          val transformer: InputTransformer =  ???
          TimingUtil.time(s"Building graphs for cluster range ($i)") {
            transformer.buildDirectedStateGraphsForAllClusterings()
          }
        }
      }.seq

    println(s"Computed clustering results.")

    clustGraphPairs.zipWithIndex.map {
      case ((clustering, graph), i) => {
        val runFolder = new File(baseOpts.reporterOpts.outFolder,
          s"synthesis-run-$i")
        val runReporter = new Reporter(baseOpts.reporterOpts.copy(
          outFolder = runFolder))

        val synthesizer = new Synthesizer(baseOpts.synthOpts, runReporter)

        // TODO store and pass graph sources here
        val results = TimingUtil.time(s"Synthesizing ($i)") {
          synthesizer.synthesizeForPositiveHardConstraints(graph)
        }

        SynthesisResultLogger(results, runReporter.file("functions.txt"))

        (clustering, results)
      }
    }
  }

  // TODO prune this
  def apply2[U](
    baseOpts: Opts,
    paramRangeExpanders: Seq[OptParameterRangeExpander[_, InputTransformerOpts]]
  ): Unit = {
    val annotCtx = AnnotationContext.fromOpts(baseOpts.annotationOpts)

    val allOpts = expandOpts(baseOpts.inputTransformerOpts, paramRangeExpanders)
    println(s"Expanded to ${allOpts.size} options.")

    val clusteringGraphPairs = ParUtil.withParallelism(8, allOpts).flatMap{
      opt => {
        // TODO: use default reporter
        val transformer: InputTransformer = ???
        transformer.buildDirectedStateGraphsForAllClusterings()
      }
    }.seq
    println(s"Computed clustering results: ${clusteringGraphPairs.size}")

    val expandedBigrams = clusteringGraphPairs.flatMap{
      case (clustering, graph) => {
        val bigramsWithoutRepetition =
          buildBigramsWithRepetition(graph).distinct
        expandBigrams(bigramsWithoutRepetition, clustering)
      }
    }.seq
    println(s"Computed expanded bigrams (${expandedBigrams.size}.")

    val withinClustPairs = clusteringGraphPairs.flatMap{
      case (clustering, _) => withinClusterPairs(clustering)
    }
    println(s"Computed within-cluster pairs (${withinClustPairs.size}")

    val bigramCounts = CollectionUtil.orderByCount(expandedBigrams)
    savePairsWithCounts(bigramCounts, new File("bigrams.csv"))

    val withinClusterCounts = CollectionUtil.orderByCount(withinClustPairs)
    savePairsWithCounts(withinClusterCounts,
      new File("within-cluster-counts.csv"))

    val combinedCounts = CollectionUtil.orderByCount(expandedBigrams ++
      withinClustPairs)
    savePairsWithCounts(combinedCounts, new File("combined-counts.csv"))
  }

  def withinClusterPairs(
    clustering: Map[String, Set[String]]
  ): Seq[(String, String)] = {
    clustering.toSeq flatMap {
      case (k, v) => withinClusterPairs(v)
    }
  }

  def withinClusterPairs(cluster: Set[String]): Seq[(String, String)] = {
    val clusterSeq = cluster.toSeq
    for {
      x <- clusterSeq
      y <- clusterSeq
      if x != y
    } yield (x, y)
  }

  def expandOpts(
    baseOpts: InputTransformerOpts,
    paramRangeExpanders: Seq[OptParameterRangeExpander[_, InputTransformerOpts]]
  ): Seq[InputTransformerOpts] = {
    def step(
      acc: List[InputTransformerOpts],
      pre: OptParameterRangeExpander[_, InputTransformerOpts]
    ): List[InputTransformerOpts] = {
      acc.flatMap(opts => pre.expand(opts))
    }

    paramRangeExpanders.foldLeft(List(baseOpts))(step)
  }

  def buildBigramsWithRepetition(
    graph: DirectedBooleanStateGraph
  ): Seq[(String, String)] = {
    graph.enumeratePathsWithLen(2) map labelPair
  }

  def labelPair(
    nodeSeq: IndexedSeq[StateGraphVertex]
  ): (String, String) = nodeSeq match {
    case IndexedSeq(v1, v2, v3) => {
      val l1 = UndirectedStateGraphOps.edgeLabels(UnlabeledEdge(v1, v2))
      val l2 = UndirectedStateGraphOps.edgeLabels(UnlabeledEdge(v2, v3))

      assert(l1.size == 1 && l2.size == 1, "Multiple labels for edge.")

      (l1.head, l2.head)
    }
    case _ => sys.error(s"Expected 3-node path, received: $nodeSeq")
  }

  def expandBigrams(
    bigrams: Seq[(String, String)], clustering: Map[String, Set[String]]
  ): Seq[(String, String)] = {
    bigrams flatMap (b => expandBigram(b, clustering))
  }

  def expandBigram(
    bigram: (String, String), clustering: Map[String, Set[String]]
  ): Seq[(String, String)] = {
    for {
      src <- clustering(bigram._1).toSeq
      tgt <- clustering(bigram._2).toSeq
    } yield {
      (src, tgt)
    }
  }

  def savePairsWithCounts(
    pairs: Seq[((String, String), Int)], f: File
  ): Unit = {
    val lines = pairs map {
      case (bigram, count) => List(bigram._1, bigram._2, count).mkString(",")
    }
    val content = lines.mkString("\n")
    FileUtil.writeToFile(f, content)
  }
}
