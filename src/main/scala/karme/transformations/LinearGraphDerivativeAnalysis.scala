package karme.transformations

import com.github.tototoshi.csv.CSVWriter
import karme.CellTrajectories.CellTrajectory
import karme.Experiments.Experiment
import karme.{CellTrajectories, PredictionLibrary, Reporter}
import karme.transformations.ExpressionDerivation.{Downregulated, ExpressionDerivative, Unchanged, Upregulated}
import karme.util.MathUtil
import karme.visualization.HistogramPlotInterface

import scala.collection.mutable.ListBuffer

class LinearGraphDerivativeAnalysis(
  distComp: DistributionComparisonTest,
  pValue: Double
)(reporter: Reporter) {

  val STRICT_COMPARISON = false

  val histogramPlotInterface = new HistogramPlotInterface

  def analyze(
    experiment: Experiment[Double],
    trajectory: CellTrajectory,
    kdExp: PredictionLibrary
  ): Unit = {
    val orderedMs = CellTrajectories.orderMeasurementsByTrajectory(experiment,
      trajectory).measurements
    val cellTree = HierarchicalCellTrees.buildCellHierarchy(orderedMs)

    val levels = 2 to 4

    val levelToDerivatives = (for (level <- levels) yield {
      val groups = HierarchicalCellTrees.findMeasurementSetsAtLevel(cellTree,
        level)
      val ds = new HierarchicalDerivatives(distComp,
        pValue).makeDerivativesPerGroup(groups)
      level -> ds
    }).toMap

//    val levelToPValues = (for (level <- levels) yield {
//      val groups = HierarchicalCellTrees.findMeasurementSetsAtLevel(cellTree,
//        level)
//      val hd = new HierarchicalDerivatives(distComp, pValue)
//      val ps = hd.makePValuesPerGroup(groups)
//      level -> ps
//    }).toMap

//    checkDerivativesAgainstKD(experiment.names, levels, levelToDerivatives,
//      levelToPValues, kdExp)
    checkPrecisionRecall(experiment.names, levels, levelToDerivatives, kdExp)
  }

  def updateCounter(
    counters: Map[String, Int], id: String
  ): Map[String, Int] = {
    counters.get(id) match {
      case Some(cnt) => counters.updated(id, cnt + 1)
      case None => counters.updated(id, 1)
    }
  }

  def checkPrecisionRecall(
    names: Seq[String],
    levels: Seq[Int],
    levelToDerivatives: Map[Int, Map[String, Seq[ExpressionDerivative]]],
    kdExp: PredictionLibrary
  ): Unit = {
    val sources = kdExp.predictions.map(_.source).distinct.sorted
    val targets = kdExp.predictions.map(_.target).distinct.sorted

    var rows = ListBuffer[List[String]]()

    var precisionValues = List[Double]()
    var recallValues = List[Double]()
    var foldEnrichmentValues = List[Double]()

    for (source <- sources) {
      val (sameSignTargets, diffSignTargets) = findAgreeingTargets(source,
        targets.toSet, levels, levelToDerivatives)
      val targetsFromDerivatives = sameSignTargets ++ diffSignTargets

      val actualTargets = findActualTargets(source, kdExp)

      val agreeingActualTargets = actualTargets.intersect(targetsFromDerivatives)

      val precision = agreeingActualTargets.size.toDouble /
        targetsFromDerivatives.size

      val backgroundRatio = actualTargets.size.toDouble / targets.size

      val foldEnrichment = precision / backgroundRatio

      val recall = agreeingActualTargets.size.toDouble / actualTargets.size

      if (!precision.isNaN) {
        precisionValues = precision :: precisionValues
      }
      recallValues = recall :: recallValues
      foldEnrichmentValues = foldEnrichment :: foldEnrichmentValues

      rows.append(List(
        source,
        precision.toString,
        recall.toString,
        foldEnrichment.toString,
        targetsFromDerivatives.toList.sorted.mkString(","),
        actualTargets.toList.sorted.mkString(",")
      ))
    }

    savePrecRecall(rows.toList)

    histogramPlotInterface.plot(precisionValues,
      reporter.file("precision-distribution.pdf"))
    histogramPlotInterface.plot(recallValues,
      reporter.file("recall-distribution.pdf"))
    histogramPlotInterface.plot(foldEnrichmentValues,
      reporter.file("fold-enrichment-distribution.pdf"))
  }

  def findAgreeingTargets(
    source: String,
    targets: Set[String],
    levels: Seq[Int],
    levelToDerivatives: Map[Int, Map[String, Seq[ExpressionDerivative]]]
  ): (Set[String], Set[String]) = {
    val sameSignTargets = targets filter { t =>
      agreesWithDerivatives(source, t, true, levels, levelToDerivatives)
    }
    val diffSignTargets = targets filter { t=>
      agreesWithDerivatives(source, t, false, levels, levelToDerivatives)
    }
    (sameSignTargets, diffSignTargets)
  }

  def findActualTargets(
    source: String,
    kdExp: PredictionLibrary
  ): Set[String] = {
    kdExp.predictions.filter(_.source == source).map(_.target).toSet
  }

  def agreesWithDerivatives(
    source: String,
    target: String,
    sameSign: Boolean,
    levels: Seq[Int],
    levelToDerivatives: Map[Int, Map[String, Seq[ExpressionDerivative]]]
  ): Boolean = {
    levelToDerivatives.exists {
      case (l, ds) => {
        val srcDsOpt = ds.get(source)
        val tgtDsOpt = ds.get(target)
        (srcDsOpt, tgtDsOpt) match {
          case (Some(srcDs), Some(tgtDs)) => {
            canPrecedeForFirstChange(srcDs, tgtDs, sameSign, STRICT_COMPARISON)
          }
          case _ => false
        }
      }
    }
  }

  def checkDerivativesAgainstKD(
    names: Seq[String],
    levels: Seq[Int],
    levelToDerivatives: Map[Int, Map[String, Seq[ExpressionDerivative]]],
    levelToPValues: Map[Int, Map[String, Seq[Double]]],
    kdExp: PredictionLibrary
  ): Unit = {
    var counters = Map[String, Int]()

    val rowBuffer = new ListBuffer[List[Any]]()

    for (p <- kdExp.predictions.sortBy(p => - math.abs(p.weight))) {
      if (names.contains(p.source) && names.contains(p.target)) {
        for (level <- levels) {
          val srcDeriv = levelToDerivatives(level)(p.source)
          val tgtDeriv = levelToDerivatives(level)(p.target)

          val srcPVals = levelToPValues(level)(p.source)
          val tgtPVals = levelToPValues(level)(p.target)

          val fwdValid = canPrecede(srcDeriv, tgtDeriv, p.weight < 0, STRICT_COMPARISON)
          val bwdValid = canPrecede(tgtDeriv, srcDeriv, p.weight < 0, STRICT_COMPARISON)

          val fwdFstChangeValid = canPrecedeForFirstChange(srcDeriv,
            tgtDeriv, p.weight < 0, STRICT_COMPARISON)
          val bwdFstChangeValid = canPrecedeForFirstChange(tgtDeriv,
            srcDeriv, p.weight < 0, STRICT_COMPARISON)

//          if (fwdValid) {
//            counters = updateCounter(counters,
//              s"level ${level} forward success")
//          } else {
//            counters = updateCounter(counters,
//              s"level ${level} forward failure")
//          }
//
//          if (bwdValid) {
//            counters = updateCounter(counters,
//              s"level ${level} backward success")
//          } else {
//            counters = updateCounter(counters,
//              s"level ${level} backward failure")
//          }

          if (fwdFstChangeValid) {
            counters = updateCounter(counters,
              s"level ${level} forward first success")
          } else {
            counters = updateCounter(counters,
              s"level ${level} forward first failure")
          }

          if (bwdFstChangeValid) {
            counters = updateCounter(counters,
              s"level ${level} backward first success")
          } else {
            counters = updateCounter(counters,
              s"level ${level} backward first failure")
          }

          val row = List(
            p.source,
            p.target,
            p.weight,
            level,
            derivString(srcDeriv),
            derivString(tgtDeriv),
            pValString(srcPVals),
            pValString(tgtPVals),
            fwdValid,
            bwdValid,
            fwdFstChangeValid,
            bwdFstChangeValid
          )
          rowBuffer.append(row)
        }
      }
    }

    saveTable(rowBuffer.toList)
    saveCounts(counters)

  }

  def derivString(ds: Seq[ExpressionDerivative]): String = {
    val ss = ds.map {
      case Upregulated => "u"
      case Downregulated => "d"
      case Unchanged => "."
    }
    ss.mkString("")
  }

  def pValString(ps: Seq[Double]): String = {
    val ss = ps.map { p => MathUtil.roundTo(4)(p) }
    ss.mkString(", ")
  }

  def firstChangeIndex(ds: Seq[ExpressionDerivative]): Int = {
    ds.indexWhere {
      case Unchanged => false
      case _ => true
    }
  }

  def canPrecede(
    srcDeriv: Seq[ExpressionDerivative],
    tgtDeriv: Seq[ExpressionDerivative],
    sameSign: Boolean,
    strict: Boolean
  ): Boolean = {
    assert(srcDeriv.size == tgtDeriv.size)
    val n = srcDeriv.size

    (0 until n).exists { i =>
      (0 until n).exists { j =>
        (if (strict) i < j else i <= j) &&
          srcDeriv(i) != Unchanged &&
          tgtDeriv(j) != Unchanged &&
          (sameSign == (srcDeriv(i) == tgtDeriv(j)))
      }
    }
  }

  def canPrecedeForFirstChange(
    srcDeriv: Seq[ExpressionDerivative],
    tgtDeriv: Seq[ExpressionDerivative],
    sameSign: Boolean,
    strict: Boolean
  ): Boolean = {
    val srcFirstChangeIdx = firstChangeIndex(srcDeriv)
    val tgtFirstChangeIdx = firstChangeIndex(tgtDeriv)

    if (srcFirstChangeIdx < 0 || tgtFirstChangeIdx < 0) {
      false
    } else {
      val precedenceOK = if (strict) {
        srcFirstChangeIdx < tgtFirstChangeIdx
      } else {
        srcFirstChangeIdx <= tgtFirstChangeIdx
      }
      val signOK =
        sameSign == (srcDeriv(srcFirstChangeIdx) == tgtDeriv(tgtFirstChangeIdx))

      precedenceOK && signOK
    }
  }

  def saveTable(rows: Seq[Seq[Any]]): Unit = {
    val headers = List(
      "source",
      "target",
      "fold change",
      "resolution",
      "source derivatives",
      "target derivatives",
      "source p-values",
      "target p-values",
      "consistent",
      "opposite consistent",
      "consistent first change",
      "opposite consistent first change"
    )

    val w = CSVWriter.open(reporter.file("derivative-analysis.csv"))
    w.writeRow(headers)
    w.writeAll(rows)
    w.close()
  }

  def saveCounts(
    counters: Map[String, Int]
  ): Unit = {
    val headers = List("counter", "count")

    val rows = counters.toList.sortBy(_._1).map {
      case (k, v) => List(k, v)
    }
    val w = CSVWriter.open(reporter.file("derivative-analysis-counts.csv"))
    w.writeRow(headers)
    w.writeAll(rows)
    w.close()
  }

  def savePrecRecall(
    rows: List[List[String]]
  ): Unit = {
    val headers = List("source", "precision", "recall", "fold enrichment",
      "agreeing targets", "actual targets")
    val w = CSVWriter.open(reporter.file("precision-recall-per-source.csv"))
    w.writeRow(headers)
    w.writeAll(rows)
    w.close()
  }
}
