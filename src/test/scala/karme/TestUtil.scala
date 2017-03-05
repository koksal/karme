package karme

import java.io.File
import java.nio.file.Files
import java.util.Arrays

object TestUtil {

  def resourceFile(name: String): File = {
    val url = getClass.getResource("/" + name)
    new File(url.getFile())
  }

  def filesHaveSameContent(f1: File, f2: File): Boolean = {
    Arrays.equals(Files.readAllBytes(f1.toPath()),
      Files.readAllBytes(f2.toPath()))
  }

  def filesHaveSameSize(f1: File, f2: File): Boolean = {
    f1.length() == f2.length()
  }

}
