package brbo.frontend

import brbo.common.MyLogger
import com.sun.source.tree.{ClassTree, CompilationUnitTree, MethodTree, Tree}
import com.sun.source.util.{SourcePositions, TreePath, TreePathScanner, Trees}
import org.apache.commons.io.FilenameUtils
import org.checkerframework.javacutil.BasicTypeProcessor

import javax.annotation.processing.SupportedAnnotationTypes
import javax.lang.model.SourceVersion
import scala.collection.JavaConverters._

@SupportedAnnotationTypes(Array("*"))
class BasicProcessor extends BasicTypeProcessor {
  private val logger = MyLogger.createLogger(classOf[BasicProcessor], debugMode = false)

  private var sourceCode: Option[String] = None
  private var compilationUnitName: Option[String] = None

  private var rootTree: Option[CompilationUnitTree] = None
  private var trees: Option[Trees] = None
  private var positions: Option[SourcePositions] = None

  private var classes = Map[ClassTree, Set[TargetMethod]]()
  private var mainMethod: Option[TargetMethod] = None

  override protected def createTreePathScanner(root: CompilationUnitTree): TreePathScanner[_, _] = {
    rootTree = Some(root)
    sourceCode = Some(root.getSourceFile.getCharContent(false).toString)
    compilationUnitName = Some(FilenameUtils.getBaseName(root.getSourceFile.getName))

    new TreePathScanner[Void, Void]() {
      override def visitClass(node: ClassTree, p: Void): Void = {
        val members = node.getMembers.asScala.toSet
        val methods: Set[TargetMethod] =
          members.filter({
            case _: MethodTree => true
            case _ => false
          }).map({
            t =>
              // If this step throws exception, then we won't get detailed error information because of the stupid CheckerFramework implementation of `typeProcess`
              val methodTree = t.asInstanceOf[MethodTree]
              val result = TargetMethod(methodTree)
              if (result.methodName == TargetProgram.MAIN_FUNCTION) {
                assert(mainMethod.isEmpty)
                mainMethod = Some(result)
              }
              result
          })
        assert(!classes.contains(node))
        classes = classes + (node -> methods)
        // super.visitClass(node, p)
        null
      }
    }
  }

  override def typeProcessingOver(): Unit = {
    val log = getCompilerLog
    if (log.nerrors > 0) {
      log.flush()
      logger.fatal(s"Compilation error in file `$getFileName`: ${log.toString}")
      throw new Exception(s"Compilation error in file `$getFileName`: ${log.toString}")
    }

    trees = Some(Trees.instance(processingEnv))
    positions = Some(trees.get.getSourcePositions)

    super.typeProcessingOver()

    // Stop execution by throwing an exception. This makes sure that compilation
    // does not proceed, and thus the AST is not modified by further phases of
    // the compilation (and we save the work to do the compilation).
    throw new EarlyStopException(s"`${classOf[BasicProcessor]}`: Stop execution by throwing an exception")
  }

  override def getSupportedSourceVersion: SourceVersion = SourceVersion.latestSupported

  def getEnclosingClass(node: MethodTree): Option[ClassTree] = {
    classes.find({
      case (_, methods) => methods.exists(targetMethod => targetMethod.methodTree == node)
    }) match {
      case Some(candidate) => Some(candidate._1)
      case None => None
    }
  }

  def getLineNumber(node: Tree): Int = {
    def getLineNumber(node: Tree, positions: SourcePositions, root: CompilationUnitTree): Long = {
      root.getLineMap.getLineNumber(positions.getStartPosition(root, node))
    }

    getLineNumber(node, positions.get, rootTree.get).toInt
  }

  private def getFileName: String = rootTree.get.getSourceFile.getName

  def getClasses: Map[ClassTree, Set[TargetMethod]] = classes

  def getCompilationUnitName: String = compilationUnitName.get

  def getSourceCode: String = sourceCode.get

  def assumeOneClass(): Unit = {
    assert(getClasses.size == 1, s"We should analyze exactly one class. Instead, we have `$getClasses`")
    // assert(getMethods.size == 1, s"We should analyze exactly one method. Instead, we have `$getMethods`")
  }

  def getPath(tree: Tree): TreePath = trees.get.getPath(rootTree.get, tree)
}

object BasicProcessor {
  private val logger = MyLogger.createLogger(BasicProcessor.getClass, debugMode = false)

  private def run(className: String, sourceFileContents: String): BasicProcessor = {
    val basicProcessor = new BasicProcessor
    try {
      JavacUtils.runProcessor(className, sourceFileContents, basicProcessor)
    }
    catch {
      case e: Exception =>
        logger.fatal(s"Compilation error `${e.getMessage}` for program:\n$sourceFileContents")
        throw e
    }
    basicProcessor
  }

  /**
   *
   * @param className          The class name
   * @param sourceFileContents Java source code that contains a class, which defines exactly 1 method
   * @return The method in the Java source code
   */
  def getTargetProgram(className: String, sourceFileContents: String): TargetProgram = {
    val processor = run(className, sourceFileContents)
    processor.assumeOneClass()
    processor.mainMethod match {
      case Some(mainMethod) =>
        TargetProgram(className, processor.classes.head._2, mainMethod, processor.getLineNumber, processor.getPath, processor.getSourceCode)
      case None =>
        throw new Exception(s"We need a main method in class `$className`, whose name must be `${TargetProgram.MAIN_FUNCTION}`!")
    }
  }
}