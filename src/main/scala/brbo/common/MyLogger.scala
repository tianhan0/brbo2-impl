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
}
