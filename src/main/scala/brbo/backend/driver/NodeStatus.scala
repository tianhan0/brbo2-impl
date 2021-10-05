package brbo.backend.driver

object NodeStatus extends Enumeration {
  type NodeStatus = Value
  val EXPLORING, SHOULD_NOT_EXPLORE = Value
}
