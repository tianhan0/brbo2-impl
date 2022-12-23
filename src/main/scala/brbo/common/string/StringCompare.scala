package brbo.common.string

import org.apache.commons.text.StringEscapeUtils

object StringCompare {
  private val dashes = "$" * 80

  def compareLiteral(actual: String, expected: String, printEscaped: Boolean, message: String = ""): Boolean = {
    val result = actual.replaceAll("\\r", "") == expected.replaceAll("\\r", "")
    val printActual =
      if (printEscaped) StringEscapeUtils.escapeJava(actual)
      else actual
    val printExpected =
      if (printEscaped) StringEscapeUtils.escapeJava(expected)
      else expected
    if (!result) {
      val lineSeparator = s"$dashes\n"
      System.err.println(s"Error message: $message\nActual:\n$printActual\n${lineSeparator}Expected:\n$printExpected\n$lineSeparator")
    }
    result
  }

  def ignoreWhitespaces(actual: String, expected: String, message: String = ""): Boolean = {
    val result = actual.replaceAll("(?s)\\s+", " ").trim == expected.replaceAll("(?s)\\s+", " ").trim
    if (!result) {
      val lineSeparator = s"$dashes\n"
      System.err.println(s"Error message: $message\nActual:\n$actual\n${lineSeparator}Expected:\n$expected\n$lineSeparator")
    }
    result
  }

  def ignoreWhitespaces(actual: Iterable[Any], expected: String, message: String): Boolean = ignoreWhitespaces(Left(actual), expected, message)

  def ignoreWhitespaces(actual: Traversable[Any], expected: String, message: String): Boolean = ignoreWhitespaces(Right(actual), expected, message)

  private def ignoreWhitespaces(actual: Either[Iterable[Any], Traversable[Any]], expected: String, message: String): Boolean = {
    ignoreWhitespaces(toSortedString(actual), expected, message)
  }

  def toSortedString(iterableOrTraversable: Either[Iterable[Any], Traversable[Any]], delimiter: String = "\n"): String = {
    iterableOrTraversable match {
      case Left(iterable) => iterable.map(x => x.toString).toList.sortWith(_ < _).mkString(delimiter)
      case Right(traversable) => traversable.map(x => x.toString).toList.sortWith(_ < _).mkString(delimiter)
    }
  }
}
