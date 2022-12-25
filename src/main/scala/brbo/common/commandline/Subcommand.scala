package brbo.common.commandline

object Subcommand {
  abstract class AbstractSubcommand

  object Fuzz extends AbstractSubcommand {
    override def toString: String = "fuzz"
  }

  object Decompose extends AbstractSubcommand {
    override def toString: String = "decompose"
  }
}
