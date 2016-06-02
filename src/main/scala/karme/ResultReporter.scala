package karme

import java.io.File

trait ResultReporter {
  def output(name: String, content: String): Unit
}

class FileReporter(
  var outFolder: File, outLabel: Option[String]
) extends ResultReporter {
  outFolder.mkdirs

  def outFile(name: String): File = {
    val fname = outLabel match {
      case Some(prefix) => s"$prefix-$name"
      case None => name
    }
    new File(outFolder, fname)
  }

  def output(name: String, content: String): Unit = {
    val f = outFile(name)
    Util.writeToFile(f, content)
    println(s"Wrote file: ${f.getAbsolutePath}")
  }
}

object FileReporter {
  def outputTuples(
    f: File,
    ts: Seq[scala.collection.Map[String, _]]
  ): Unit = {
    val cols = ts.head.keySet.toList
    val rows = for (t <- ts) yield {
      cols map (col => t(col))
    }
    writeMatrix(f, cols +: rows)
  }

  def writeTable[A,B,C](
    f: File,
    rowName: String,
    colName: String,
    rowHeaders: Seq[A],
    colHeaders: Seq[B],
    values: Seq[Seq[C]]
  ): Unit = {
    val firstCell = s"$rowName \\ $colName"
    val firstRow = firstCell +: colHeaders
    val remainingRows = rowHeaders.zip(values).map{ case (rh, rvs) =>
      rh +: rvs
    }
    writeMatrix(f, firstRow +: remainingRows)
  }

  def writeMatrix[T](f: File, m: Seq[Seq[T]]): Unit = {
    val writer = com.github.tototoshi.csv.CSVWriter.open(f)
    writer.writeAll(m)
    writer.close()
  }

  def writeVector(xs: Seq[Double], f: File): Unit = {
    Util.writeToFile(f, xs.mkString("\n"))
  }
}

class ConsoleReporter extends ResultReporter {
  def output(name: String, content: String): Unit = {
    println(s"Output: $name")
    println(content)
  }
}

class NoopReporter extends ResultReporter {
  def output(name: String, content: String): Unit = { }
}
