import scala.util.parsing.combinator.RegexParsers

class ASTBuilder extends RegexParsers {
  protected def astNodeParser: Parser[AstNode] = printlnNode | variableDefenition | functionDefenition | variableRedefenition | functionCall | expr | varLoad
  protected def astParser: Parser[ScopeNode] = rep(astNodeParser) ^^ { v => ScopeNode(v) } /*functionDefenition | functionCall | variableDefenition | structDefenition*/
  protected def functionDefenition: Parser[FunctionDefNode] = (("def" ~> ident) ~ (params <~ "{") ~ (astParser <~ "}")) ^^ {p =>
      p._1 match {
        case id ~ ls => FunctionDefNode(id, ls.map(s => Pair(s, None)), p._2)
      }}
  protected def functionCall: Parser[FunctionCallNode] = ident ~ commaSeparatedExprs ^^ {p =>
    FunctionCallNode(p._1, p._2)}
  protected def commaSeparatedExprs: Parser[List[AstNode]] = "(" ~> repsep(astNodeParser, ",") <~ ")"
  protected def params: Parser[List[String]] = "(" ~> repsep(ident, ",") <~ ")"
//  def functionDefenition: Parser[FunctionDefenitionNode[_]] =  parse fun def*/
  protected def varLoad: Parser[LoadVarNode] = ident ^^ {v => LoadVarNode(v) }
  protected def printlnNode: Parser[PrintlnCallNode] = "println" ~> ("(" ~> astNodeParser <~ ")") ^^ { v => PrintlnCallNode(v) }
  protected def literalNode: Parser[LiteralNode] = integerParser | doubleParser | stringLiteralParser
  protected def integerParser: Parser[IntLiteralNode] = integer ^^ { v => IntLiteralNode(BigInt(v)) }
  protected def doubleParser: Parser[DoubleLiteralNode] = double ^^ { v => DoubleLiteralNode(BigDecimal(v)) }
  protected def stringParser: Parser[String] ="""[^"]*""".r
  protected def stringLiteralParser: Parser[StringLiteralNode] = "\"" ~> stringParser <~ "\"" ^^ { v => StringLiteralNode(v) }
  protected def variableDefenition: Parser[DefVarNode] = "var" ~> ident ~ ("=" ~> expr) ^^ {p => DefVarNode(p._1, p._2)
    }
  protected def variableRedefenition: Parser[StoreVarNode] = (ident <~ "=") ~ expr ^^ {p => StoreVarNode(p._1, p._2) }

  def expr: Parser[AstNode] = term ~ rep("+" ~ term | "-" ~ term) ^^ toBinary
  protected def term: Parser[AstNode] = factor ~ rep("*" ~ factor | "/" ~ factor) ^^ toBinary
  protected def factor: Parser[AstNode] = literalNode | varLoad | variableDefenition |
    structDefenition | ("(" ~> expr <~ literal(")"))

  protected def structDefenition: Parser[StructLiteralNode] = ("{" ~> rep(variableDefenition)) <~ "}" ^^ {
    p => StructLiteralNode(p.map(f => (f.name, f.value)).toMap)
  }

  protected def toBinary: PartialFunction[~[AstNode, List[~[String, AstNode]]], AstNode] = {
    case left ~ list =>
      if (list.isEmpty) left
      else list.foldLeft(left) {
        case (res, op ~ right) => BinaryOperationNode(res, op ,right)
      }
  }

  def parse(source: String) = parseAll(astParser, source) match {
    case Success(res, _) => Some(res)
    case r @ _ => 
      println(r)
      None
  }

  protected def integer: Parser[String] = """-?0|([1-9]\d*)""".r
  protected def double: Parser[String] = """-?0|([1-9]\d*)\.\d*""".r
  protected def ident: Parser[String] = """[a-zA-Z_][a-zA-Z0-9_]*""".r
}

object ASTBuilder {
  def buildAST(program: String) = (new ASTBuilder).parse(program)
}

