package karme.transformations

import karme.Experiments.BooleanExperiment
import karme.Reporter
import karme.util.{FileUtil, MathUtil}

class DifferentialGeneFiltering(minRatio: Double)(implicit reporter: Reporter) {

  def filterSymmetric(exp: BooleanExperiment): BooleanExperiment = {
    val differentialNames = findSymmetricDifferentialNames(exp)

    exp.project(differentialNames)
  }

  private def findSymmetricDifferentialNames(
    exp: BooleanExperiment
  ): Set[String] = {
    val highValueRatios = exp.names.map(n => n -> highValueRatio(n, exp))

    val differentialNames = highValueRatios.collect{
      case (name, ratio) if highRatioIsWithinBounds(ratio, minRatio) => name
    }.toSet

    logHighValueRatios(highValueRatios)
    logFilteringRatio(exp, differentialNames)

    differentialNames
  }

  private def highRatioIsWithinBounds(
    highRatio: Double, threshold: Double
  ): Boolean = {
    highRatio >= threshold && highRatio <= (1 - threshold)
  }

  private def highValueRatio(name: String, exp: BooleanExperiment): Double = {
    val vs = exp.valuesForName(name)
    val nbHigh = vs.count(v => v)
    nbHigh.toDouble / vs.size
  }

  private def logFilteringRatio(
    exp: BooleanExperiment, filteredNames: Set[String]
  ): Unit = {
    val f = reporter.file("differential-expression-filtering-result.txt")

    val remainingGeneRatio = filteredNames.size.toDouble / exp.names.size

    FileUtil.writeToFile(f,
      s"Ratio of remaining genes: ${remainingGeneRatio}\n")
  }

  private def logHighValueRatios(
    highValueRatios: Seq[(String, Double)]
  ): Unit = {
    val f = reporter.file("differential-expression-high-value-ratios.txt")

    val sb = new StringBuilder()

    for ((name, ratio) <- highValueRatios.sortBy(_._2).reverse) {
      sb append (s"$name: ${MathUtil.roundTo(ratio, 4)}\n")
    }

    FileUtil.writeToFile(f, sb.toString())
  }
}
