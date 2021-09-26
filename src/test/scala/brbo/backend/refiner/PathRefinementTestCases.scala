package brbo.backend.refiner

import brbo.backend.verifier.cex.Path
import brbo.common.BrboType.{INT, VOID}
import brbo.common.GhostVariableTyp.Resource
import brbo.common.GhostVariableUtils
import brbo.common.ast.{Addition, Assignment, Block, BrboFunction, BrboProgram, Identifier, Number, Reset, Skip, Use}
import brbo.common.cfg.CFGNode

object PathRefinementTestCases {
  val brboProgram: BrboProgram = BrboProgram("Test program", BrboFunction("Main", VOID, Nil, Block(List(Skip()))))

  private val R = Identifier(GhostVariableUtils.generateName("", Resource), INT)
  private val update = Addition(R, Number(0))
  private val assignment = Assignment(R, update)
  private val assignmentNode = CFGNode(Left(Assignment(Identifier("x", INT), Number(0))), brboProgram.mainFunction, CFGNode.DONT_CARE_ID)
  private val use1 = CFGNode(Left(Use(Some(1), update)), brboProgram.mainFunction, CFGNode.DONT_CARE_ID)
  private val use2 = CFGNode(Left(Use(Some(2), update)), brboProgram.mainFunction, CFGNode.DONT_CARE_ID)
  // private val use3 = CFGNode(Left(Use(Some(3), update, assignment)), brboProgram.mainFunction, CFGNode.DONT_CARE_ID)
  private val reset1 = CFGNode(Left(Reset(1)), brboProgram.mainFunction, CFGNode.DONT_CARE_ID)
  private val reset2 = CFGNode(Left(Reset(2)), brboProgram.mainFunction, CFGNode.DONT_CARE_ID)

  val test01: Path = Path(List(assignmentNode, use1))
  val test02: Path = Path(List(use1, assignmentNode))
  val test03: Path = Path(List(use1, assignmentNode, use2))
  val test04: Path = Path(List(assignmentNode, use1, use2))
  val test05: Path = Path(List(use1, use2, assignmentNode))
  val test06: Path = Path(List(assignmentNode, use1, assignmentNode, use2, assignmentNode))

  val test07: Path = Path(List(reset1, use1, assignmentNode))
  val test08: Path = Path(List(reset1, use1, reset1, assignmentNode))
  val test09: Path = Path(List(reset1, use1, reset1, assignmentNode, reset1))
}
