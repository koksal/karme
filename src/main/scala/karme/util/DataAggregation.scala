package karme.util

import java.io.File

object DataAggregation {

  val columnsToAggregate = Set("Precision", "Recall", "F1 Score", "Similarity")

  def parentTo2DPoint(f: File): Map[String, Double] = {
    val parentName = f.getAbsoluteFile.getParentFile.getName
    val pairFormat = raw"(\w+)=([0-9\.]+)-(\w+)=([0-9\.]+)".r
    parentName match {
      case pairFormat(label1, value1, label2, value2) => {
        Map(label1 -> value1.toDouble, label2 -> value2.toDouble)
      }
      case _ => sys.error(s"Cannot match 2D point from filename: ${f.getName}")
    }
  }

  def grandParentToKeyValue(f: File): (String, String) = {
    val grandParentName = f.getAbsoluteFile.getParentFile.getParentFile.getName
    val keyValueFormat = raw"(\w+)=(\w*)".r
    grandParentName match {
      case keyValueFormat(key, value) => (key, value)
      case _ => sys.error(s"Cannot match key-value from filename: ${f.getName}")
    }
  }

}
