package brbo.backend.refiner

import brbo.backend.verifier.cex.Path
import brbo.common.BrboType.{INT, VOID}
import brbo.common.ast._
import brbo.common.cfg.CFGNode

object PathRefinementTestCases {
  val brboProgram: BrboProgram = BrboProgram("Test program", BrboFunction("Main", VOID, Nil, Block(List(Skip())), Set(0, 1, 2)))

  private val update = Number(1)
  private val assignmentNode = CFGNode(Left(Assignment(Identifier("x", INT), Number(0))), brboProgram.mainFunction, CFGNode.DONT_CARE_ID)
  private val use1 = CFGNode(Left(Use(Some(1), update)), brboProgram.mainFunction, CFGNode.DONT_CARE_ID)
  private val use2 = CFGNode(Left(Use(Some(2), update)), brboProgram.mainFunction, CFGNode.DONT_CARE_ID)
  // private val use3 = CFGNode(Left(Use(Some(3), update, assignment)), brboProgram.mainFunction, CFGNode.DONT_CARE_ID)
  private val reset1 = CFGNode(Left(Reset(1)), brboProgram.mainFunction, CFGNode.DONT_CARE_ID)
  private val reset2 = CFGNode(Left(Reset(2)), brboProgram.mainFunction, CFGNode.DONT_CARE_ID)

  val test01: Path = Path(List(reset1, reset1, use1)) // A segment with no uses!
  val test02: Path = Path(List(reset1, use1, reset1, use1))
  val test03: Path = Path(List(reset1, use1, reset1, use1, reset1, use1))
  val test04: Path = Path(List(reset1, use1, reset1, use1, reset2, use2, reset2, use2))
  val test05: Path = Path(List(reset1, use1, assignmentNode, reset1, use1))
  val test06: Path = Path(List(reset2, use2, reset1, use1, reset1, use1, reset2, use2))

  val test07: Refinement = {
    val path = List(reset1, use1, assignmentNode)
    Refinement(path, Map(), Set(), Map())
  }
  val test08: Refinement = {
    val path = List(reset1, use1, reset1, assignmentNode)
    Refinement(path, Map(), Set(), Map())
  }
  val test09: Refinement = {
    val path = List(reset1, use1, reset1, assignmentNode, reset1)
    Refinement(path, Map(), Set(), Map())
  }
}
