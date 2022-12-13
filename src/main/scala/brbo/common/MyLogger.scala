package brbo.common

import org.apache.logging.log4j.{LogManager, Logger}

class MyLogger(logger: Logger, val debugMode: Boolean) {
  def traceOrError(message: String): Unit = {
    if (debugMode) logger.error(message)
    else logger.trace(message)
  }

  def infoOrError(message: String): Unit = {
    if (debugMode) logger.error(message)
    else logger.info(message)
  }

  def debug(message: String): Unit = logger.debug(message)

  def trace(message: String): Unit = logger.trace(message)

  def fatal(message: String): Unit = logger.fatal(message)

  def fatal(message: String, t: Throwable): Unit = logger.fatal(message, t)

  def info(message: String): Unit = logger.info(message)

  def error(message: String): Unit = logger.error(message)

  def warn(message: String): Unit = logger.warn(message)
}

object MyLogger {
  def createLogger[T](clazz: Class[T], debugMode: Boolean): MyLogger = new MyLogger(LogManager.getLogger(clazz), debugMode)

  val sharedDebugLogger: MyLogger = createLogger(MyLogger.getClass, debugMode = true)

  val sharedLogger: MyLogger = createLogger(MyLogger.getClass, debugMode = false)
}