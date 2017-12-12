package karme.util

import java.io.File

object TimingUtil {

  def time[T](description: String)(code: => T): T = {
    val start = System.currentTimeMillis
    val result = code
    val end = System.currentTimeMillis
    val durationInMs = end - start
    println(s"$description: $durationInMs ms")
    result
  }

  def log[T](description: String, file: File)(code: => T): T = {
    val start = System.currentTimeMillis
    val result = code
    val end = System.currentTimeMillis
    val durationInMs = end - start
    FileUtil.writeToFile(
      file,
      s"$description\t$durationInMs\n",
      append = true
    )
    result
  }

}
