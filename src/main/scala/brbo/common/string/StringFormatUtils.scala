package brbo.common.string

import org.apache.commons.text.WordUtils

object StringFormatUtils {
  def float(d: Double, digit: Int = 1): String = s"%.${digit}f" format d

  def integer(i: Int, digit: Int = 3): String = s"%0${digit}d".format(i)

  /**
   *
   * @param columns A list of pairs of a string in a column and the width of the column
   * @return
   */
  def wrapColumnsFixedWidth(columns: List[(String, Int)]): String = {
    var maxLines = 0
    val indentLength = 2
    val lines: List[Array[String]] = columns.map({
      case (column, wrapLength) =>
        val lines = WordUtils.wrap(column, wrapLength, "\n", false).split("\n")
        if (lines.length > maxLines) maxLines = lines.length
        lines
    })
    Range(0, maxLines).map({
      lineIndex =>
        val columnsAtThisLine = columns.indices.map({
          columnIndex =>
            val wrapLength = columns(columnIndex)._2
            val columnAsLines = lines(columnIndex)
            val indent = if (lineIndex == 0) "" else " " * indentLength
            val columnAtThisLine =
              if (lineIndex >= columnAsLines.length) ""
              else columnAsLines(lineIndex)
            (indent + columnAtThisLine, wrapLength)
        })
        // Left aligned
        val format = columnsAtThisLine.map(pair => s"%-${pair._2}s").mkString(" ")
        String.format(format, columnsAtThisLine.map(pair => pair._1): _*)
    }).mkString("\n")
  }

  def formatSize(v: Long): String = {
    if (v < 1024) return v + " B"
    val z = (63 - java.lang.Long.numberOfLeadingZeros(v)) / 10
    val size = float(v.toDouble / (1L << (z * 10)))
    val unit = " KMGTPE".charAt(z)
    s"$size$unit"
  }

  def prependIndentsPerLine(string: String, indent: Int): String = {
    val indentString = " " * indent
    indentString + string.replaceAll("\\n", s"\n$indentString")
  }
}
