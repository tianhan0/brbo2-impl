package brbo.common

object FileUtils {
  def readFromFile(location: String): String = {
    val source = scala.io.Source.fromFile(location)
    try source.mkString finally source.close()
  }
}
