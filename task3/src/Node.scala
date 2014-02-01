
trait Node

//class FunctionDefenitionNode[R](arguments: Seq[ClassManifest[_]], body: => R) extends Node

class VariableDefenition(val name: String, val node: Node) extends Node {
  override def toString = "val " + name + " = " + node.toString
}
class Flow(val flow: Seq[Node]) extends Node { override def toString = flow.mkString("\n") }
trait Literal extends Node
class IntLiteral(val value: String) extends Literal{ override def toString = value }
class DoubleLiteral(val value: String) extends Literal{ override def toString = value }
class StringLiteral(val value: String) extends Literal{ override def toString = value }