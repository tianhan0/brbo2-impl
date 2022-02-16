package brbo.backend.verifier.modelchecker

import brbo.backend.verifier.{Verifier, VerifierResult}
import brbo.common.CommandLineArguments
import brbo.common.ast.{BrboProgram, BrboProgramInC}

class ModelChecker(override val arguments: CommandLineArguments) extends Verifier {
  override val toolName: String = "BasicModelChecker"
  override val toolDirectory: String = ""

  override def verify(program: BrboProgram): VerifierResult = {
    ???
  }
}
