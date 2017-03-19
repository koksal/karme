package karme

import java.io.File

class Reporter(outputFolder: File) {

  outputFolder.mkdirs()

  def file(name: String): File = {
    new File(outputFolder, name)
  }

  def emit(message: Any): Unit = {
    println(message)
  }

}
