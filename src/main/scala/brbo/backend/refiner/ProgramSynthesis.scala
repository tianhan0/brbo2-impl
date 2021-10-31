package brbo.backend.refiner

import brbo.common.ast._
import brbo.common.{CommandLineArguments, MyLogger}

class ProgramSynthesis(commandLineArguments: CommandLineArguments) {
  private val logger = MyLogger.createLogger(classOf[ProgramSynthesis], commandLineArguments.getDebugMode)

  def synthesize(brboProgram: BrboProgram, refinement: Refinement): Option[BrboProgram] = {
    logger.info(s"Synthesize a new main function from a refinement.")
    val allCommands = BrboAstUtils.collectCommands(brboProgram.mainFunction.body)
    val useCommands = allCommands.filter(command => command.isInstanceOf[Use])
    val resetCommands = allCommands.filter(command => command.isInstanceOf[Reset])

    useCommands.foreach({
      useCommand =>
        refinement.getSplitUseInstances(useCommand.asInstanceOf[Use], ???)
    })

    ???
  }
}
