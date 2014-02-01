//// Ast nodes

class VarLoad {}
class FunctionNode {}
class LoadVar {}
class StoreVar {}


///

class ExecutionResult(value: Any, isFail:Boolean = false) {
    
}



class Context(val parentContext : Context) {
  
  def this() = {
    this(null)
  }
  
  var vars : Map[String, String] = Map()
  // TODO: map to real signatures
  var functions : Map[String, FunctionNode] = Map()  
  
  
  
}

class Interpreter(ast: Node) {
  
  def exec() : ExecutionResult = exec(new Context(), ast)
  
  private def exec(context:Context, node: Node) : ExecutionResult = {
    node match {
      
      case literal: Literal => execLiteral(literal)
      case flow : Flow => execFlow(flow)
      case _ => throw new IllegalArgumentException("I don't know this kind of node")
      
    }
  }
  
  private def execLiteral(literal : Literal) : ExecutionResult = {
    return null
  }
  
  // will return the last expression
  private def execFlow(flow : Flow) : ExecutionResult = {
    return null
  }
  
  
  
}

object main extends App{

  val program =
    """val a = {123}
      |val c = {"adfasdf"}
      |val b = {val c = {"hello"}}
    """.stripMargin
  
  val ast = ASTBuilder.buildAST(program)
  
  val int = new Interpreter(new IntLiteral("12"))
  int.exec()

  println("hi, man")
  
}

