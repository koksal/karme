package karme.transformations

import com.github.tototoshi.csv.CSVWriter
import karme.CellTrajectories
import karme.CellTrajectories.CellTrajectory
import karme.Experiments.Experiment
import karme.PredictionLibrary
import karme.Reporter
import karme.transformations.ExpressionDerivation.Downregulated
import karme.transformations.ExpressionDerivation.ExpressionDerivative
import karme.transformations.ExpressionDerivation.Unchanged
import karme.transformations.ExpressionDerivation.Upregulated
import karme.util.MathUtil

import scala.collection.mutable.ListBuffer

class LinearGraphDerivativeAnalysis(
  distComp: DistributionComparisonTest
)(reporter: Reporter) {

  val pValue = 0.01

  def analyze(
    experiment: Experiment[Double],
    trajectory: CellTrajectory,
    kdExp: PredictionLibrary
  ): Unit = {
    val orderedMs = CellTrajectories.orderMeasurementsByTrajectory(experiment,
      trajectory).measurements
    val cellTree = HierarchicalCellTrees.buildCellHierarchy(orderedMs)

    val levels = 1 to 3

    val levelToDerivatives = (for (level <- levels) yield {
      val groups = HierarchicalCellTrees.findMeasurementSetsAtLevel(cellTree,
        level)
      val ds = new HierarchicalDerivatives(distComp,
        pValue).makeDerivativesPerGroup(groups)
      level -> ds
    }).toMap

    val levelToPValues = (for (level <- levels) yield {
      val groups = HierarchicalCellTrees.findMeasurementSetsAtLevel(cellTree,
        level)
      val hd = new HierarchicalDerivatives(distComp, pValue)
      val ps = hd.makePValuesPerGroup(groups)
      level -> ps
    }).toMap

    checkDerivativesAgainstKD(experiment.names, levels, levelToDerivatives,
      levelToPValues, kdExp)
  }

  def updateCounter(
    counters: Map[String, Int], id: String
  ): Map[String, Int] = {
    counters.get(id) match {
      case Some(cnt) => counters.updated(id, cnt + 1)
      case None => counters.updated(id, 1)
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

          val strict = true
          val fwdValid = canPrecede(srcDeriv, tgtDeriv, p.weight < 0, strict)
          val bwdValid = canPrecede(tgtDeriv, srcDeriv, p.weight < 0, strict)

          val fwdBestP = findBestPrecedencePValue(srcDeriv, tgtDeriv,
            srcPVals, tgtPVals, p.weight < 0, strict)
          val bwdBestP = findBestPrecedencePValue(tgtDeriv, srcDeriv,
            tgtPVals, srcPVals, p.weight < 0, strict)

          if (fwdValid) {
            counters = updateCounter(counters,
              s"level ${level} forward success")
          } else {
            counters = updateCounter(counters,
              s"level ${level} forward failure")
          }

          if (bwdValid) {
            counters = updateCounter(counters,
              s"level ${level} backward success")
          } else {
            counters = updateCounter(counters,
              s"level ${level} backward failure")
          }

          if (fwdValid && bwdValid) {
            if (fwdBestP.get == bwdBestP.get) {
              counters = updateCounter(counters,
                s"level ${level} forward p-value tie")
            } else if (fwdBestP.get < bwdBestP.get) {
              counters = updateCounter(counters,
                s"level ${level} forward p-value success")
            } else {
              counters = updateCounter(counters,
                s"level ${level} forward p-value failure")
            }
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
            fwdBestP.getOrElse(" "),
            bwdBestP.getOrElse(" ")
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

  def lastChangeIndex(ds: Seq[ExpressionDerivative]): Int = {
    ds.lastIndexWhere {
      case Unchanged => false
      case _ => true
    }
  }

  def findBestPrecedencePValue(
    srcDeriv: Seq[ExpressionDerivative],
    tgtDeriv: Seq[ExpressionDerivative],
    srcPVals: Seq[Double],
    tgtPVals: Seq[Double],
    sameSign: Boolean,
    strict: Boolean
  ): Option[Double] = {
    assert(srcDeriv.size == tgtDeriv.size)
    assert(srcPVals.size == tgtPVals.size)
    assert(srcDeriv.size == srcPVals.size)
    val n = srcDeriv.size

    var validPValues = Set[Double]()
    for (i <- 0 until n) {
      for (j <- 0 until n) {
        val canPrecede = (if (strict) i < j else i <= j) &&
          srcDeriv(i) != Unchanged &&
          tgtDeriv(j) != Unchanged &&
          (sameSign == (srcDeriv(i) == tgtDeriv(j)))
        if (canPrecede) {
          validPValues += math.min(srcPVals(i), tgtPVals(j))
        }
      }
    }

    if (validPValues.isEmpty) {
      None
    } else {
      Some(validPValues.min)
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
      "best p-value",
      "opposite best p-value"
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
}
