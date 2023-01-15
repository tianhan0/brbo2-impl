package brbo.backend.refiner

import brbo.backend.verifier.cex.Path
import brbo.common.BrboType.{INT, VOID}
import brbo.common.GhostVariableUtils
import brbo.common.ast._
import brbo.common.cfg.CFGNode

object PathRefinementTestCases {
  val brboProgram: BrboProgram = BrboProgram("Test program", packageName = None, BrboFunction("Main", VOID, Nil, Block(List(Skip())), Set(0, 1, 2), useResource = false))

  private val r1Initialization = GhostVariableUtils.declareVariables(1, legacy = false).map(c => CFGNode(c, Some(brboProgram.mainFunction), CFGNode.DONT_CARE_ID))
  private val r2Initialization = GhostVariableUtils.declareVariables(2, legacy = false).map(c => CFGNode(c, Some(brboProgram.mainFunction), CFGNode.DONT_CARE_ID))
  private val initializations = r1Initialization ::: r2Initialization
  private val update = Number(1)
  private val assignmentNode = CFGNode(Assignment(Identifier("x", INT), Number(0)), Some(brboProgram.mainFunction), CFGNode.DONT_CARE_ID)
  private val use1 = CFGNode(Use(Some(1), update), Some(brboProgram.mainFunction), CFGNode.DONT_CARE_ID)
  private val use2 = CFGNode(Use(Some(2), update), Some(brboProgram.mainFunction), CFGNode.DONT_CARE_ID)
  // private val use3 = CFGNode(Left(Use(Some(3), update, assignment)), brboProgram.mainFunction, CFGNode.DONT_CARE_ID)
  private val reset1 = CFGNode(Reset(1), Some(brboProgram.mainFunction), CFGNode.DONT_CARE_ID)
  private val reset2 = CFGNode(Reset(2), Some(brboProgram.mainFunction), CFGNode.DONT_CARE_ID)

  val test01: Path = Path(r1Initialization ::: List(reset1, reset1, use1)) // A segment with no uses!
  val test02: Path = Path(r1Initialization ::: List(reset1, use1, reset1, use1))
  val test03: Path = Path(r1Initialization ::: List(reset1, use1, reset1, use1, reset1, use1))
  val test04: Path = Path(initializations ::: List(reset1, use1, reset1, use1, reset2, use2, reset2, use2))
  val test05: Path = Path(r1Initialization ::: List(reset1, use1, assignmentNode, reset1, use1))
  val test06: Path = Path(initializations ::: List(reset2, use2, reset1, use1, reset1, use1, reset2, use2))

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
