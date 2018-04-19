package karme.util

import java.io.File

import karme.evaluation.synthetic.ClassificationEval

object DataAggregation {

  val columnsToAggregate = Set("Precision", "Recall", "F1 Score", "Similarity")

  def grandParentTo2DPoint(f: File): Map[String, Double] = {
    nameTo2DPoint(FileUtil.getParent(FileUtil.getParent(f)).getName)
  }

  def parentTo2DPoint(f: File): Map[String, Double] = {
    nameTo2DPoint(FileUtil.getParent(f).getName)
  }

  def nameTo2DPoint(name: String): Map[String, Double] = {
    val pairFormat = raw"(\w+)=([0-9\.]+)-(\w+)=([0-9\.]+)".r
    name match {
      case pairFormat(label1, value1, label2, value2) => {
        Map(label1 -> value1.toDouble, label2 -> value2.toDouble)
      }
      case _ => sys.error(s"Cannot match 2D point from name: ${name}")
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

  def twoDimPointToColor(point: Map[String, Double]): String = {
    val values = point.values.toList
    assert(values.size == 2)
    val f1 = ClassificationEval.getF1Score(1 - values(0), 1 - values(1))
    MathUtil.roundTo(1)(f1).toString
  }
}
