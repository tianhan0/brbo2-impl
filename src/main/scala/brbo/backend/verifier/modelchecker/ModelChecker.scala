package brbo.backend.verifier.modelchecker

import brbo.backend.verifier.{Verifier, VerifierResult}
import brbo.common.ast.{BrboCProgram, BrboProgram}
import brbo.common.commandline.Arguments

class ModelChecker(override val arguments: Arguments) extends Verifier {
  override val toolName: String = "BasicModelChecker"
  override val toolDirectory: String = ""

  override def verify(program: BrboProgram): VerifierResult = {
    ???
  }
}
