package karme

import java.io.File

class Reporter(val opts: ReporterOpts) {

  opts.outFolder.mkdirs()

  def file(name: String): File = {
    new File(opts.outFolder, name)
  }

  def log(message: Any): Unit = {
    println(message)
  }

  def debug(message: Any): Unit = {
    if (opts.verbose) {
      log(message)
    }
  }

  def subfolderReporter(subfolderName: String): Reporter = {
    new Reporter(opts.copy(outFolder = this.file(subfolderName)))
  }

}

object Reporter {

  def defaultReporter(): Reporter = new Reporter(ReporterOpts())

}
