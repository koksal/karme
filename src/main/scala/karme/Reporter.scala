package karme

import java.io.File

class Reporter(opts: ReporterOpts) {

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

}

object Reporter {

  def defaultReporter(): Reporter = new Reporter(ReporterOpts())

}
