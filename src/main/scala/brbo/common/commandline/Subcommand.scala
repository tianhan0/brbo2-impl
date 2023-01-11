package brbo.common.commandline

object Subcommand {
  abstract class AbstractSubcommand

  object Fuzz extends AbstractSubcommand {
    override def toString: String = "fuzz"
  }

  object Decompose extends AbstractSubcommand {
    override def toString: String = "decompose"
  }

  // Transform the input Java program into an acceptable form for brbo
  object TransformToBrbo extends AbstractSubcommand {
    override def toString: String = "brbo"
  }
}
