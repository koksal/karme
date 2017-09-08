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

  val pValue = 0.05

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

  def checkDerivativesAgainstKD(
    names: Seq[String],
    levels: Seq[Int],
    levelToDerivatives: Map[Int, Map[String, Seq[ExpressionDerivative]]],
    kdExp: PredictionLibrary
  ): Unit = {
    var levelToSuccessCounter = levels.map(l => l -> 0).toMap
    var levelToFailureCounter = levels.map(l => l -> 0).toMap

    val rowBuffer = new ListBuffer[List[Any]]()

    for (p <- kdExp.predictions.sortBy(p => - math.abs(p.weight))) {
      if (names.contains(p.source) && names.contains(p.target)) {
        for (level <- levels) {
          val srcDeriv = levelToDerivatives(level)(p.source)
          val tgtDeriv = levelToDerivatives(level)(p.target)

          val validData = canPrecede(srcDeriv, tgtDeriv, p.weight < 0)
          if (validData) {
            levelToSuccessCounter += level -> (levelToSuccessCounter(level) + 1)
          } else {
            levelToFailureCounter += level -> (levelToFailureCounter(level) + 1)
          }

          val row = List(
            p.source,
            p.target,
            p.weight,
            level,
            derivString(srcDeriv),
            derivString(tgtDeriv),
            validData
          )
          rowBuffer.append(row)
        }
      }
    }

    saveTable(rowBuffer.toList)
    saveCounts(levelToSuccessCounter, levelToFailureCounter)

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
    sameSign: Boolean
  ): Boolean = {
    assert(srcDeriv.size == tgtDeriv.size)
    val n = srcDeriv.size

    (0 until n).exists { i =>
      (0 until n).exists { j =>
        i <= j &&
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
      "consistent"
    )

    val w = CSVWriter.open(reporter.file("derivative-analysis.csv"))
    w.writeRow(headers)
    w.writeAll(rows)
    w.close()
  }

  def saveCounts(
    levelToSuccessCounter: Map[Int, Int],
    levelToFailureCounter: Map[Int, Int]
  ): Unit = {
    val successRows = levelToSuccessCounter.map {
      case (level, count) => List("success", level, count)
    }

    val failureRows = levelToFailureCounter.map {
      case (level, count) => List("failure", level, count)
    }

    val headers = List("outcome", "resolution", "count")

    val w = CSVWriter.open(reporter.file("derivative-analysis-counts.csv"))
    w.writeRow(headers)
    w.writeAll(successRows.toList ++ failureRows.toList)
    w.close()
  }
}
