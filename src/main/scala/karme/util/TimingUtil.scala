package karme.util

object TimingUtil {

  def time[T](description: String)(code: => T): T = {
    val start = System.currentTimeMillis
    val result = code
    val end = System.currentTimeMillis
    val durationInMs = end - start
    println(s"$description: $durationInMs ms")
    result
  }

}
