package brbo

import brbo.common.BrboType
import brbo.common.BrboType.{INT, VOID}
import brbo.common.ast._
import brbo.common.cfg.CFGNode
import org.scalatest.flatspec.AnyFlatSpec

import java.io.{File, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}
import java.nio.file.Files


class SerializationTest extends AnyFlatSpec {
  "Serializing and deserializing" should "be correct" in {
    val fileName = s"${classOf[SerializationTest].getCanonicalName}.txt"
    val fileOutputStream = new FileOutputStream(fileName)
    val objectOutputStream = new ObjectOutputStream(fileOutputStream)

    val function: BrboFunction = {
      val n: Identifier = Identifier("n", INT)
      val k: Identifier = Identifier("k", INT)
      val declaration2 = VariableDeclaration(k, Number(0))
      val reset2 = Reset(1)
      val use2 = Use(Some(1), Number(1))
      val updateK1 = Assignment(k, Addition(k, Number(1)))
      val reset3 = Reset(1)
      val use3 = Use(Some(1), Number(2))
      val updateK2 = Assignment(k, Addition(k, Number(2)))
      val reset4 = Reset(1)
      val use4 = Use(Some(1), Subtraction(n, k))
      BrboFunction("main", VOID, List(Identifier("n", BrboType.INT)),
        Block(List(declaration2, reset2, use2, updateK1, reset3, use3, updateK2, reset4, use4)), Set(1), useResource = false)
    }
    val program: BrboProgram = BrboProgram("Test program", packageName = None, function)
    val cfgNode = {
      val assignment = Assignment(Identifier("x", BrboType.INT), Addition(Number(1), Identifier("y", BrboType.INT)))
      CFGNode(assignment, Some(function))
    }

    objectOutputStream.writeObject(cfgNode)
    objectOutputStream.writeObject(function)
    objectOutputStream.writeObject(program)
    objectOutputStream.flush()
    objectOutputStream.close()

    val fileInputStream = new FileInputStream(fileName)
    val objectInputStream = new ObjectInputStream(fileInputStream)

    val cfgNode2 = objectInputStream.readObject.asInstanceOf[CFGNode]
    val function2 = objectInputStream.readObject.asInstanceOf[BrboFunction]
    val program2 = objectInputStream.readObject.asInstanceOf[BrboProgram]

    objectInputStream.close()

    assert(cfgNode.sameValue(cfgNode2))
    assert(function.sameAs(function2))
    assert(program.sameAs(program2))

    val file = new File(fileName)
    Files.deleteIfExists(file.toPath)
  }
}
