package brbo.backend.verifier

import brbo.common.TypeUtils.BrboType.BOOL
import brbo.common.ast._
import brbo.common.cfg.CFGNode
import org.apache.logging.log4j.LogManager
import org.jgrapht.Graph
import org.jgrapht.graph.DefaultEdge
import org.jgrapht.graph.builder.GraphTypeBuilder
import org.jgrapht.nio.graphml.GraphMLImporter
import org.jgrapht.nio.{Attribute, ImportException}
import org.jgrapht.traverse.TopologicalOrderIterator
import org.jgrapht.util.SupplierUtil

import java.util.function.Supplier
import scala.collection.JavaConverters._
import scala.collection.mutable

case class CounterexamplePath(nodes: List[CFGNode]) {
  nodes.map(node => node.value).foreach({
    case Left(_) =>
    case Right(expr) => assert(expr.typ == BOOL)
  })
}

object CounterexamplePath {
  private val logger = LogManager.getLogger("brbo.backend.verifier.CounterexamplePath")

  def graphMLToCounterexamplePath(graphMLString: String, brboProgram: BrboProgram): CounterexamplePath = {
    val vertexAttributes = new mutable.HashMap[String, mutable.HashMap[String, Attribute]]
    val edgeAttributes = new mutable.HashMap[DefaultEdge, mutable.HashMap[String, Attribute]]
    val graph = readGraph(new java.io.StringReader(graphMLString), classOf[DefaultEdge], directed = true, weighted = false, vertexAttributes, edgeAttributes)

    var path: List[String] = Nil
    var lastVertex: Option[String] = None
    val iterator = new TopologicalOrderIterator(graph)
    while (iterator.hasNext) {
      val current = iterator.next()
      lastVertex match {
        case Some(last) =>
          // Get the edge between the last and the current nodes
          val edges = graph.getAllEdges(last, current).asScala
          assert(edges.size == 1)

          edgeAttributes.get(edges.head) match {
            case Some(attributes) =>
              attributes.get("sourcecode") match {
                case Some(sourcecode) => path = sourcecode.toString :: path
                case None => throw new Exception
              }
            case None =>
          }
        case None =>
      }
      lastVertex = Some(current)
    }
    path = path.reverse
    logger.error(path)

    parsePathString(path, brboProgram)
  }

  /**
   *
   * @param path A counterexample path, which is a list of strings
   * @param brboProgram The function to parse the counterexample path against
   * @return Parse strings into BrboAst or BrboExpr by matching against the pretty print strings of the entry function
   */
  private def parsePathString(path: List[String], brboProgram: BrboProgram): CounterexamplePath = {
    def helper(path: List[String], continuation: Set[Statement]): CounterexamplePath = {
      path match {
        case Nil => CounterexamplePath(Nil)
        case ::(head, tail) =>
          if (head.startsWith("[") && head.endsWith("]")) {
            ???
          }
          else {
            ???
          }
          val counterexamplePath = helper(tail, ???)
          CounterexamplePath(??? :: counterexamplePath.nodes)
      }
    }

    helper(path, Set(brboProgram.mainFunction.body))
  }

  // Copied from https://github.com/jgrapht/jgrapht/blob/6aba8e81053660997fe681c50974c07e312027d1/jgrapht-io/src/test/java/org/jgrapht/nio/graphml/GraphMLImporterTest.java
  @throws[ImportException]
  private def readGraph[E](input: java.io.StringReader, edgeClass: Class[E],
                           directed: Boolean, weighted: Boolean,
                           vertexAttributes: mutable.HashMap[String, mutable.HashMap[String, Attribute]],
                           edgeAttributes: mutable.HashMap[E, mutable.HashMap[String, Attribute]]): Graph[String, E] = {
    val stringSupplier: Supplier[String] = SupplierUtil.createStringSupplier(1)
    val g: Graph[String, E] =
      if (directed) GraphTypeBuilder.directed[String, E].allowingMultipleEdges(true).allowingSelfLoops(true).weighted(weighted).vertexSupplier(stringSupplier).edgeClass(edgeClass).buildGraph
      else GraphTypeBuilder.undirected[String, E].allowingMultipleEdges(true).allowingSelfLoops(true).weighted(weighted).vertexSupplier(stringSupplier).edgeClass(edgeClass).buildGraph

    val importer = new GraphMLImporter[String, E]
    importer.addVertexAttributeConsumer({
      case (k: org.jgrapht.alg.util.Pair[String, String], a: Attribute) =>
        val vertex = k.getFirst
        val attrs =
          vertexAttributes.get(vertex) match {
            case Some(attrs) => attrs
            case None =>
              val attrs = new mutable.HashMap[String, Attribute]
              vertexAttributes.put(vertex, attrs)
              attrs
          }
        attrs.put(k.getSecond, a)
    })

    importer.addEdgeAttributeConsumer({
      case (k: org.jgrapht.alg.util.Pair[E, String], a: Attribute) =>
        val edge = k.getFirst
        val attrs =
          edgeAttributes.get(edge) match {
            case Some(attrs) => attrs
            case None =>
              val attrs = new mutable.HashMap[String, Attribute]
              edgeAttributes.put(edge, attrs)
              attrs
          }
        attrs.put(k.getSecond, a)
    })

    importer.importGraph(g, input)
    g
  }
}