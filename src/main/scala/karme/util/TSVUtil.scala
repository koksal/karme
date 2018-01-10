package karme.util

import java.io.File

import com.github.tototoshi.csv.CSVReader
import com.github.tototoshi.csv.CSVWriter
import com.github.tototoshi.csv.TSVFormat

object TSVUtil {

  def readHeadersAndData(f: File): (List[String], List[Map[String, String]]) = {
    val reader = CSVReader.open(f)(new TSVFormat {})
    reader.allWithOrderedHeaders()
  }

  def filterByHeaders(
    tuples: Seq[Map[String, Any]], selectedHeaders: Seq[String]
  ): Seq[Seq[Any]] = {
    tuples map { t =>
      selectedHeaders map (h => t(h))
    }
  }

  def saveOrderedTuples(
    headers: Seq[String], tuples: Seq[Seq[Any]], f: File
  ): Unit = {
    val writer = CSVWriter.open(f)(new TSVFormat {})
    writer.writeRow(headers)
    writer.writeAll(tuples)
    writer.close()
  }

  def saveTupleMaps(
    tupleMaps: Seq[Map[String, Any]], f: File
  ): Unit = {
    if (tupleMaps.nonEmpty) {
      saveTupleMapsWithOrderedHeaders(
        tupleMaps.head.keySet.toList.sorted, tupleMaps, f)
    }
  }

  def saveTupleMapsWithOrderedHeaders(
    headers: Seq[String], tupleMaps: Seq[Map[String, Any]], f: File
  ): Unit = {
    val orderedTuples = tupleMaps map { m =>
      headers map (h => m(h))
    }
    saveOrderedTuples(headers, orderedTuples, f)
  }

  def takeRowsMin(
    colsToExpand: Seq[String],
    rows: Seq[Map[String, Any]]
  ): Seq[Map[String, Any]] = {
    reduceRows(xs => xs.min, colsToExpand, rows)
  }

  def takeRowsMax(
    colsToExpand: Seq[String],
    rows: Seq[Map[String, Any]]
  ): Seq[Map[String, Any]] = {
    reduceRows(xs => xs.max, colsToExpand, rows)
  }

  def takeRowsMedian(
    colsToExpand: Seq[String],
    rows: Seq[Map[String, Any]]
  ): Seq[Map[String, Any]] = {
    reduceRows(xs => MathUtil.median(xs), colsToExpand, rows)
  }

  def reduceRows(
    f: Seq[Double] => Any,
    colsToExpand: Seq[String],
    rows: Seq[Map[String, Any]]
  ): Seq[Map[String, Any]] = {
    val colsToReduce: Seq[String] = rows.headOption.map{ row =>
      row.keySet -- colsToExpand.toSet
    }.getOrElse(Nil).toSeq

    val grouped = rows.groupBy(row => colsToExpand.map(row(_))).toList

    for ((_, groupRows) <- grouped) yield {
      val reducedPairs = for (c <- colsToReduce) yield {
        val reduced = f(groupRows.map(r => r(c).toString.toDouble))
        c -> reduced
      }
      val expandedPairs = colsToExpand.map { c =>
        c -> groupRows.head(c)
      }
      (reducedPairs ++ expandedPairs).toMap
    }
  }
}
