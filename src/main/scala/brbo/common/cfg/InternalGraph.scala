package brbo.common.cfg

// Root and exit nodes of a CFG for a function or an AST,
// in a CFG that contains all nodes for all functions in a program
case class InternalGraph(root: CFGNode, exits: Set[CFGNode])
