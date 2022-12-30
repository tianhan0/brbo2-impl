package brbo.common.string

import brbo.BrboMain
import org.apache.commons.text.StringEscapeUtils

import java.nio.file.{Files, Paths}

object StringCompare {
  private val dashes = "$" * 80

  def compareLiteral(actual: String, expected: String, printEscaped: Boolean, message: String = ""): Boolean = {
    val newActual = replaceCarriageReturn(actual)
    val newExpected = replaceCarriageReturn(expected)
    val result = newActual == newExpected
    if (!result) {
      val printActual =
        if (printEscaped) StringEscapeUtils.escapeJava(newActual)
        else newActual
      val printExpected =
        if (printEscaped) StringEscapeUtils.escapeJava(newExpected)
        else newExpected
      val lineSeparator = s"$dashes\n"
      System.err.println(s"Error message: $message\nActual:\n$printActual\n${lineSeparator}Expected:\n$printExpected\n$lineSeparator\n${diffSummary(actual = actual, expected = expected)}")
    }
    result
  }

  private def diffSummary(actual: String, expected: String): String = {
    val expectedFile = Paths.get(s"${BrboMain.OUTPUT_DIRECTORY}/expected.txt")
    val actualFile = Paths.get(s"${BrboMain.OUTPUT_DIRECTORY}/actual.txt")
    BrboMain.writeToFile(path = expectedFile, content = expected)
    BrboMain.writeToFile(path = actualFile, content = actual)
    val diffSummary = BrboMain.diffFiles(expectedFile, actualFile)
    Files.deleteIfExists(expectedFile)
    Files.deleteIfExists(actualFile)
    diffSummary
  }

  private def replaceCarriageReturn(string: String): String = string.replaceAll("\\r\\n", "\n")

  def ignoreWhitespaces(actual: String, expected: String, message: String = ""): Boolean = {
    val result = actual.replaceAll("(?s)\\s+", " ").trim == expected.replaceAll("(?s)\\s+", " ").trim
    if (!result) {
      val lineSeparator = s"$dashes\n"
      System.err.println(s"Error message: $message\nActual:\n$actual\n${lineSeparator}Expected:\n$expected\n$lineSeparator\n${diffSummary(actual = actual, expected = expected)}")
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
