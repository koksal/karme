package karme.util

import java.io.File

import karme.visualization.Heatmap

object HeatmapAggregation {

  val heatmap = new Heatmap()

  def main(args: Array[String]): Unit = {
    val (outFilePrefix, restArgs) = (args.head, args.tail)

    val files = restArgs map (a => new File(a))
    val points = files map fileToPoint

    val labels = points.head.keySet.toList.sorted
    assert(labels.size == 2)

    val xLabel = labels(0)
    val yLabel = labels(1)

    val labelToValues = labels.map{
      label => label -> points.map(p => p(label)).toList.distinct.sorted
    }.toMap
    assert(points.forall(p => p.keySet == labels.toSet))

    val data = files map TSVUtil.readHeadersAndData

    val headerRows = data.map(_._1)
    assert(headerRows.toSet.size == 1,
      s"Different headers: ${headerRows.toSet}")
    val headers = headerRows.head
    val headersToAggregate = headers.toSet.intersect(
      DataAggregation.columnsToAggregate)

    val pointDataPairs = points.zip(data.map(_._2))

    for (header <- headersToAggregate) {
      val headerData = pointDataPairs.map{
        case (point, data) => point -> data.map(v => v(header).toDouble)
      }.toMap

      val matrix = prepareMatrix(labels(0), labels(1), labelToValues,
        headerData)

      val outFile = new File(s"$outFilePrefix-$header-heatmap.pdf")

      if (matrix.flatten.distinct.size > 1) {
        heatmap.plot(
          matrix,
          sanitizeLabel(xLabel),
          sanitizeLabel(yLabel),
          labelToValues(xLabel).map(_.toString),
          labelToValues(yLabel).map(_.toString),
          Some((0, 1)),
          outFile
        )
      } else {
        println(s"Skipping $outFile because there is only one distinct value.")
      }
    }

  }

  private def fileToPoint(f: File): Map[String, Double] = {
    val parentName = f.getAbsoluteFile.getParentFile.getName
    val pairFormat = raw"(\w+)=([0-9\.]+)-(\w+)=([0-9\.]+)".r
    parentName match {
      case pairFormat(label1, value1, label2, value2) => {
        Map(label1 -> value1.toDouble, label2 -> value2.toDouble)
      }
    }
  }

  def prepareMatrix(
    xLabel: String,
    yLabel: String,
    labelToValues: Map[String, Seq[Double]],
    pointDataPairs: Map[Map[String, Double], Seq[Double]]
  ): Seq[Seq[Double]] = {
    for (yVal <- labelToValues(yLabel)) yield {
      for (xVal <- labelToValues(xLabel)) yield {
        val point = Map(xLabel -> xVal, yLabel -> yVal)
        val values = pointDataPairs(point)
        MathUtil.median(values)
      }
    }
  }

  def sanitizeLabel(label: String): String = {
    label.replaceAll("_", " ")
  }

}
