import NodeState.NodeState

/**
  * @author Benedikt Zoennchen
  */
object NodeState extends Enumeration {
  type NodeState = Value
  val CONVERGED, NARROWBAND, UNKNOWN, UNREACHABLE = Value
}

class Node(state: NodeState, potential: Double) {}