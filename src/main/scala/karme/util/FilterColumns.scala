package karme.util

import java.io.File

import com.github.tototoshi.csv.CSVReader
import com.github.tototoshi.csv.CSVWriter
import com.github.tototoshi.csv.TSVFormat

object FilterColumns {

  def main(args: Array[String]): Unit = {
    val dataFile = new File(args(0))
    val columnsFile = new File(args(1))

    println("Reading data.")
    val (dataHeaders, dataTuples) = readData(dataFile)
    println("Reading columns.")
    val columnsToRetain = readColumns(columnsFile)

    val firstHeader = dataHeaders.head
    val selectedHeaders = firstHeader +: columnsToRetain

    println("Filtering data.")
    val filteredData = filterData(dataTuples, selectedHeaders)
    println("Saving data.")
    saveData(selectedHeaders, filteredData, new File("filtered.tsv"))
  }

  def readColumns(f: File): Seq[String] = {
    val reader = CSVReader.open(f)(new TSVFormat {})
    val tuples = reader.all()
    reader.close()
    tuples.map(t => t(0))
  }

  def readData(f: File): (List[String], List[Map[String, String]]) = {
    val reader = CSVReader.open(f)(new TSVFormat {})
    reader.allWithOrderedHeaders()
  }

  def filterData(
    tuples: Seq[Map[String, String]], selectedHeaders: Seq[String]
  ): Seq[Seq[String]] = {
    tuples map { t =>
      selectedHeaders map (h => t(h))
    }
  }

  def saveData(
    headers: Seq[String], tuples: Seq[Seq[String]], f: File
  ): Unit = {
    val writer = CSVWriter.open(f)(new TSVFormat {})
    writer.writeRow(headers)
    writer.writeAll(tuples)
    writer.close()
  }
}
