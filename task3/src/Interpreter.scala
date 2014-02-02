
class VarValue(stringRep: String) {
  override def toString = stringRep
}

case class IntVarValue(value: BigInt) extends VarValue(value.toString())

case class DoubleVarValue(value: BigDecimal) extends VarValue(value.toString)

case class StringVarValue(value: String) extends VarValue(value.toString)

class StructVarValue(value: Map[String, VarValue]) extends VarValue(value.toString())


class Context(val parentContext: Context) {

  def this() = {
    this(null)
  }

  var vars: Map[String, VarValue] = Map()
  var functions: Map[String, FunctionDefNode] = Map()

}

class Interpreter(ast: AstNode) {

  def exec(): Option[VarValue] = exec(new Context(), ast)


  private def exec(context: Context, node: AstNode): Option[VarValue] = {

    node match {

      case funCall: FunctionCallNode => execFunctionCall(context, funCall)
      case funDef: FunctionDefNode => execFunctionDef(context, funDef)
      case loadVar: LoadVarNode => execLoadVar(context, loadVar)
      case storeVar: StoreVarNode => execStoreVar(context, storeVar)
      case scope: ScopeNode => execScope(new Context(context), scope)
      case biOp: BinaryOperationNode => execBinaryOperation(context, biOp)
      case prt: PrintlnCallNode => execPrintNode(context, prt)
      case dv: DefVarNode => execDefVar(context, dv)
      case lit: StructLiteralNode => execStruct(context, lit)
      case lit: IntLiteralNode => Some(new IntVarValue(lit.value))
      case lit: DoubleLiteralNode => Some(new DoubleVarValue(lit.value))
      case lit: StringLiteralNode => Some(new StringVarValue(lit.value))

      case _ => throw new IllegalArgumentException("I don't know this kind of node")

    }
  }

  // todo to companion obj
  private def findFunction(context: Context, name: String): Option[FunctionDefNode] = {
    if (context == null) {
      None
    } else {
      context.functions.get(name) match {
        case fun: Some[FunctionDefNode] => fun
        case None => findFunction(context.parentContext, name)
      }
    }
  }

  // todo to companion obj
  private def findVar(context: Context, name: String): Option[VarValue] = {
    if (context == null) {
      None
    } else {
      context.vars.get(name) match {
        case vr: Some[VarValue] => vr
        case None => findVar(context.parentContext, name)
      }
    }
  }

  private def execDefVar(context: Context, dv: DefVarNode) = {
    exec(context, dv.value) match {
      case None => None
      case Some(v) => {
        context.vars += dv.name -> v
        Some(v)
      }
    }
  }

  private def execPrintNode(context: Context, prt: PrintlnCallNode) = {
    exec(context, prt.arg) match {
      case None => None
      case Some(v) => {
        println(v.toString)
        None
      }
    }
  }

  private def execFunctionCall(context: Context, funCall: FunctionCallNode) = {
    val fun = findFunction(context, funCall.name)
    if (fun.isEmpty) {
      None
    } else {
      val innerContext = new Context(context)
      for ((pair, ast) <- fun.get.args zip funCall.args) {
        exec(context, ast) match {
          case None => None
          case Some(res) => innerContext.vars += pair._1 -> res
        }
      }
      fun.get.args.drop(funCall.args.length).foreach(pair => {
        // I know that the last get will be fine
        innerContext.vars += pair._1 -> exec(context, pair._2.get).get
      })
      val kkk = execScope(innerContext, fun.get.scope)
      kkk
    }
  }

  private def execFunctionDef(context: Context, funDef: FunctionDefNode) = {
    context.functions += funDef.name -> funDef
    None
  }

  private def execLoadVar(context: Context, loadVar: LoadVarNode) = {
    findVar(context, loadVar.name) match {
      case some: Some[VarValue] => some
      case None => None
    }
  }

  private def execStoreVar(context: Context, storeVar: StoreVarNode): Option[VarValue] = {
    if (context == null) {
      None
    } else {
      val res = exec(context, storeVar.value)
      context.vars.get(storeVar.name) match {
        case vr: Some[VarValue] => {
          context.vars = context.vars.updated(storeVar.name, res.get)
          res
        }
        case None => execStoreVar(context.parentContext, storeVar)
      }
    }
  }

  private def execScope(context: Context, scope: ScopeNode) = {
    val ls = scope.nodes
    val last = ls.takeRight(1)(0)
    val head = ls.dropRight(1)
    head.foreach(node => {
      exec(context, node)
    })
    exec(context, last)
  }


  // plz close your eyes 
  private def getUpperType(a: VarValue, b: VarValue): Pair[VarValue, VarValue] = {
    a match {
      case vva: IntVarValue => {
        b match {
          case vvb: IntVarValue => Pair(vva, vvb)
          case vvb: DoubleVarValue => Pair(DoubleVarValue(BigDecimal(vva.value)), vvb)
          case vvb: StringVarValue => Pair(StringVarValue(vva.value.toString()), vvb)
        }
      }
      case vva: DoubleVarValue => {
        b match {
          case vvb: IntVarValue => Pair(vva, DoubleVarValue(BigDecimal(vvb.value)))
          case vvb: DoubleVarValue => Pair(vva, vvb)
          case vvb: StringVarValue => Pair(StringVarValue(vva.value.toString()), vvb)
        }
      }
      case vva: StringVarValue => {
        b match {
          case vvb: IntVarValue => Pair(vva, StringVarValue(vvb.value.toString()))
          case vvb: DoubleVarValue => Pair(vva, StringVarValue(vvb.value.toString()))
          case vvb: StringVarValue => Pair(vva, vvb)
        }
      }
      case _ => null
    }
  }


  private def execBinaryOperation(context: Context, biOp: BinaryOperationNode) = {
    val left = exec(context, biOp.left).get
    val right = exec(context, biOp.right).get
    val pr = getUpperType(left, right)
    Option(biOp.op match {
      case "+" => pr match {
        case Pair(IntVarValue(va), IntVarValue(vb)) => IntVarValue(va + vb)
        case Pair(DoubleVarValue(va), DoubleVarValue(vb)) => DoubleVarValue(va + vb)
        case Pair(StringVarValue(va), StringVarValue(vb)) => StringVarValue(va + vb)
      }
      case "-" => pr match {
        case Pair(IntVarValue(va), IntVarValue(vb)) => IntVarValue(va - vb)
        case Pair(DoubleVarValue(va), DoubleVarValue(vb)) => DoubleVarValue(va - vb)
      }
      case "*" => pr match {
        case Pair(IntVarValue(va), IntVarValue(vb)) => IntVarValue(va * vb)
        case Pair(DoubleVarValue(va), DoubleVarValue(vb)) => DoubleVarValue(va * vb)
      }
      case "/" => pr match {
        case Pair(IntVarValue(va), IntVarValue(vb)) => IntVarValue(va - vb)
        case Pair(DoubleVarValue(va), DoubleVarValue(vb)) => DoubleVarValue(va - vb)
      }
      case _ => null
    })


  }

  // now open
  private def execStruct(context: Context, structDef: StructLiteralNode) = {
    val nv = structDef.value.map(p => {
      p._1 -> exec(context, p._2).get
    })
    Option(new StructVarValue(nv))
  }


}

object main extends App {

  val program =
    """
      |def foo() {
      | println("HER")
      | 23
      |}
      |println(foo())
      |println(2 + 23)
      |var struct = {
      | var x = 1
      | var xb = 3
      |}
      |println(struct)
    """.stripMargin
  ASTBuilder.buildAST(program).map(f => new Interpreter(f).exec())

}

