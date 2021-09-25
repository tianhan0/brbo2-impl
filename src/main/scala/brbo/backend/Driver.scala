package brbo.backend

import brbo.backend.refiner.Refiner
import brbo.backend.verifier.VerifierRawResult._
import brbo.backend.verifier.{UAutomizerVerifier, VerifierResult}
import brbo.common.CommandLineArguments
import brbo.common.ast.{BrboExpr, BrboProgram}

class Driver(commandLineArguments: CommandLineArguments, brboProgram: BrboProgram) {
  def verifyFullyAmortize(boundAssertion: BrboExpr): VerifierResult = {
    ???
  }

  def verifyWorstCase(boundAssertion: BrboExpr): VerifierResult = {
    ???
  }

  def insertUBCheck(brboProgram2: BrboProgram, boundAssertion: BrboExpr): BrboProgram = {
    ???
  }

  def verifySelectivelyAmortize(boundAssertion: BrboExpr): VerifierResult = {
    val UAutomizerVerifier = new UAutomizerVerifier(commandLineArguments)
    val refiner = new Refiner(brboProgram, commandLineArguments)
    var shouldExit = false
    var program2 = brboProgram
    var result = VerifierResult(UNINITIALIZED, None)
    while (!shouldExit) {
      val ubCheckInserted = insertUBCheck(program2, boundAssertion)
      result = UAutomizerVerifier.verify(ubCheckInserted)
      result.rawResult match {
        case TRUE_RESULT => shouldExit = true
        case FALSE_RESULT => program2 = refiner.refine(program2, result.counterexamplePath)
        case UNKNOWN_RESULT => program2 = refiner.refine(program2, result.counterexamplePath)
        case UNINITIALIZED => throw new Exception
      }
    }
    result
  }
}
