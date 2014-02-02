//class FunctionDefenitionNode[R](arguments: Seq[ClassManifest[_]], body: => R) extends Node

/*
class VariableDefenition(val name: String, val node: Node) extends Node {
  override def toString = "val " + name + " = " + node.toString
}
class Flow(val flow: Seq[Node]) extends Node { override def toString = flow.mkString("\n") }
sealed trait Literal extends Node
class IntLiteral(val value: String) extends Literal{ override def toString = value }
class DoubleLiteral(val value: String) extends Literal{ override def toString = value }
class StringLiteral(val value: String) extends Literal{ override def toString = value }
sealed trait Expr extends Node {
  def eval: () => Either[String, Literal]
}
class BinaryExpr(val left: Expr, val op: (left.type, right.type) => Literal, val right: Expr)*/

sealed trait AstNode

case class PrintlnCallNode(val arg: AstNode) extends AstNode

case class BinaryOperationNode(val left: AstNode, val op: String, val right: AstNode) extends AstNode

case class FunctionCallNode(val name: String, val args: List[AstNode]) extends AstNode

case class FunctionDefNode(val name: String, val args: List[Pair[String, Option[LiteralNode]]],
                      val scope: ScopeNode) extends AstNode

case class LoadVarNode(val name: String) extends AstNode

case class DefVarNode(val name: String, val value: AstNode) extends AstNode

case class StoreVarNode(val name: String, val value: AstNode) extends AstNode

case class ScopeNode(val nodes: List[AstNode]) extends AstNode

sealed trait LiteralNode extends AstNode

case class IntLiteralNode(val value: BigInt) extends LiteralNode

case class DoubleLiteralNode(val value: BigDecimal) extends LiteralNode

case class StringLiteralNode(val value: String) extends LiteralNode

case class StructLiteralNode(val value: Map[String, AstNode]) extends LiteralNode