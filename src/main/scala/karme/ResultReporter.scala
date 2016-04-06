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

  def outputTuples(
    name: String,
    ts: Seq[Map[String, String]]
  ): Unit = {
    val cols = ts.head.keySet.toList
    val rows = for (t <- ts) yield {
      cols map (col => t(col))
    }
    val f = outFile(name)
    val writer = com.github.tototoshi.csv.CSVWriter.open(f)
    writer.writeAll(cols +: rows)
    writer.close()
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
