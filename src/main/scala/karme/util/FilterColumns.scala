package karme.util

import java.io.File

import com.github.tototoshi.csv.{CSVReader, CSVWriter, TSVFormat}

object FilterColumns {

  def main(args: Array[String]): Unit = {
    val dataFile = new File(args(0))
    val columnsFile = new File(args(1))

    println("Reading data.")
    val (dataHeaders, dataTuples) = TSVUtil.readHeadersAndData(dataFile)
    println("Reading columns.")
    val columnsToRetain = readColumns(columnsFile)

    val firstHeader = dataHeaders.head
    val selectedHeaders = firstHeader +: columnsToRetain

    println("Filtering data.")
    val filteredData = TSVUtil.filterByHeaders(dataTuples, selectedHeaders)
    println("Saving data.")
    TSVUtil.saveOrderedTuples(selectedHeaders, filteredData, new File("filtered.tsv"))
  }

  def readColumns(f: File): Seq[String] = {
    val reader = CSVReader.open(f)(new TSVFormat {})
    val tuples = reader.all()
    reader.close()
    tuples.map(t => t(0))
  }

}
