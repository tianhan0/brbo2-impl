package brbo.common

import org.apache.logging.log4j.Logger

case class MyLogger(logger: Logger, debugMode: Boolean) {
  def traceOrError(message: String): Unit = {
    if (debugMode) logger.error(message)
    else logger.trace(message)
  }

  def intoOrError(message: String): Unit = {
    if (debugMode) logger.error(message)
    else logger.info(message)
  }

  def debug(message: String): Unit = logger.debug(message)

  def trace(message: String): Unit = logger.trace(message)

  def fatal(message: String): Unit = logger.fatal(message)

  def fatal(message: String, t: Throwable): Unit = logger.fatal(message, t)

  def info(message: String): Unit = logger.info(message)

  def error(message: String): Unit = logger.error(message)
}
