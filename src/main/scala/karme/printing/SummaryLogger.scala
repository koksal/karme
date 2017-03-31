package karme.printing

import java.io.File

import karme.Opts
import karme.evaluation.ReachabilityEvaluation.ReachabilityEvaluationResult
import karme.evaluation.enrichr.EnrichrPredictionLibrary
import karme.util.FileUtil
import karme.util.MathUtil

object SummaryLogger {

  def apply(
    opts: Opts,
    optimalResults: Seq[ReachabilityEvaluationResult],
    refPValues: Seq[(EnrichrPredictionLibrary, Double)],
    f: File
  ): Unit = {
    val optHeaderToValue = Seq(
      "pseudolog" -> opts.inputTransformerOpts.pseudoLogFactor,
      "bool-norm" -> opts.inputTransformerOpts.booleanNormalizationMethod,
      "cell-activity" -> opts.inputTransformerOpts.cellActivityThreshold,
      "uncertainty" -> opts.inputTransformerOpts.uncertaintyThreshold,
      "smoothing" -> opts.inputTransformerOpts.smoothingRadius,
      "minClust" -> opts.inputTransformerOpts.clusteringOpts.minNbClusters,
      "maxClust" -> opts.inputTransformerOpts.clusteringOpts.maxNbClusters,
      "libraryPreds" -> opts.evalOpts.maxNbReferencePredictions
    )

    val statToValue = Seq(
      "avg. reachability" ->
        MathUtil.mean(optimalResults.map(_.observedStateReachabilityRatio))
    )

    val refHeaderToValue = refPValues map {
      case (library, pValue) => library.id -> pValue
    }

    val allValues = optHeaderToValue ++ statToValue ++ refHeaderToValue

    val sb = new StringBuilder()
    sb.append(allValues.map(_._1).mkString("\t"))
    sb.append("\n")
    sb.append(allValues.map(_._2).mkString("\t"))
    sb.append("\n")
    FileUtil.writeToFile(f, sb.toString())
  }

}
