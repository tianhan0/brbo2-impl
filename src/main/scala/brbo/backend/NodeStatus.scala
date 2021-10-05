package brbo.backend

object NodeStatus extends Enumeration {
  type NodeStatus = Value
  val EXPLORING, SHOULD_NOT_EXPLORE = Value
}
