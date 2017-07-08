package karme.transformations

import karme.Experiments.BooleanExperiment
import karme.Reporter
import karme.util.FileUtil

class DifferentialGeneFiltering(minRatio: Double)(implicit reporter: Reporter) {

  def filterSymmetric(exp: BooleanExperiment): BooleanExperiment = {
    val differentialNames = findSymmetricDifferentialNames(exp)

    logResults(exp.names.toSet, differentialNames)

    exp.project(differentialNames)
  }

  def findSymmetricDifferentialNames(exp: BooleanExperiment): Set[String] = {
    val differentialNs = exp.names filter { name =>
      val vs = exp.valuesForName(name)
      val nbHigh = vs.count(v => v)
      val nbLow = vs.count(v => !v)

      val highRatio = nbHigh.toDouble / vs.size
      val lowRatio = nbLow.toDouble / vs.size

      highRatio >= minRatio && lowRatio >= minRatio
    }
    differentialNs.toSet
  }

  def logResults(
    unfilteredNames: Set[String], filteredNames: Set[String]
  ): Unit = {
    val f = reporter.file("differential-filtering.txt")

    val filterRatio = filteredNames.size.toDouble / unfilteredNames.size
    val discardedNames = unfilteredNames -- filteredNames

    val sb = new StringBuilder()

    sb append s"Differential gene ratio: $filterRatio\n"
    sb append "Remaining genes:\n"
    appendSortedNames(sb, filteredNames)
    sb append "Discarded genes:\n"
    appendSortedNames(sb, discardedNames)

    FileUtil.writeToFile(f, sb.toString())
  }

  def appendSortedNames(sb: StringBuilder, names: Set[String]): Unit = {
    sb.append(names.toList.sorted.mkString("\n") + "\n")
  }
}
