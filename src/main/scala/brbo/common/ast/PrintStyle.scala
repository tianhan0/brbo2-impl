package brbo.common.ast

object PrintStyle {
  abstract class AbstractStyle

  // TODO: We do not actually need this style
  object CStyle extends AbstractStyle

  object BrboJavaStyle extends AbstractStyle

  object QFuzzJavaStyle extends AbstractStyle
}
