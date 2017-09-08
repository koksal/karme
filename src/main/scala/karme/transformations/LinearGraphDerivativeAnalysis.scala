package karme.transformations

import java.io.File

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

    checkDerivativesAgainstKD(experiment.names, levels, levelToDerivatives,
      kdExp)
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
    kdExp: PredictionLibrary
  ): Unit = {
    var counters = Map[String, Int]()

    val rowBuffer = new ListBuffer[List[Any]]()

    for (p <- kdExp.predictions.sortBy(p => - math.abs(p.weight))) {
      if (names.contains(p.source) && names.contains(p.target)) {
        for (level <- levels) {
          val srcDeriv = levelToDerivatives(level)(p.source)
          val tgtDeriv = levelToDerivatives(level)(p.target)

          val strict = false
          val fwdValid = canPrecede(srcDeriv, tgtDeriv, p.weight < 0, strict)
          val bwdValid = canPrecede(tgtDeriv, srcDeriv, p.weight < 0, strict)
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

          val row = List(
            p.source,
            p.target,
            p.weight,
            level,
            derivString(srcDeriv),
            derivString(tgtDeriv),
            fwdValid,
            bwdValid
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
      "consistent",
      "opposite consistent"
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
