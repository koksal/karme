package karme.util

import java.io.File

import com.github.tototoshi.csv.{CSVReader, CSVWriter, TSVFormat}

object Transpose {

  def main(args: Array[String]): Unit = {
    val f = new File((args(0)))
    val reader = CSVReader.open(f)(new TSVFormat {})
    val tuples = reader.all()

    val transposedTuples = tuples.transpose

    val writer = CSVWriter.open("transposed.csv")
    writer.writeAll(transposedTuples)
  }

}
