import scala.util.parsing.combinator.RegexParsers

trait LanguageSpecificParsers extends RegexParsers {
  def integer: Parser[String] = """-?0|([1-9]\d*)""".r
  def double: Parser[String] = """-?0|([1-9]\d*)\.\d*""".r
  def ident: Parser[String] = """[a-zA-Z_][a-zA-Z0-9_]*""".r
}

class ASTBuilder extends LanguageSpecificParsers {
  def astParser: Parser[Flow] = rep(variableDefenition | literalNode) ^^ { v => new Flow(v) } /*functionDefenition | functionCall | variableDefenition | structDefenition*/
  /*def functionDefenition: Parser[FunctionDefenitionNode[_]] = "def" ~> ident ~ (params <~ "{") ~ (body <~ "}")
  def params: Parser[(Seq[C])]
  def functionDefenition: Parser[FunctionDefenitionNode[_]] =  parse fun def*/
  def literalNode: Parser[Literal] = integerParser | doubleParser | stringLiteralParser
  def integerParser: Parser[IntLiteral] = integer ^^ { v => new IntLiteral(v) }
  def doubleParser: Parser[DoubleLiteral] = double ^^ { v => new DoubleLiteral(v) }
  def stringParser: Parser[String] ="""[^"]*""".r
  def stringLiteralParser: Parser[StringLiteral] = "\"" ~> stringParser <~ "\"" ^^ { v => new StringLiteral(v) }
  def variableDefenition: Parser[VariableDefenition] = "val" ~> ident ~ ("=" ~> "{" ~> astParser) <~ "}" ^^ {
      p => new VariableDefenition(p._1, p._2)
    }

  def parse(source: String) = parseAll(astParser, source)  match {
    case Success(result, next) => {
      println("RESULT:")
      println(result.flow)
    }
    case r @ _ => println(r)
  }
}

object ASTBuilder {
  def buildAST(program: String) = (new ASTBuilder).parse(program)
}

