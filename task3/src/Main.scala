object Main extends App {
  val program =
    """val a = {123}
      |val c = {"adfasdf"}
      |val b = {val c = {"hello"}}
    """.stripMargin
//  val program ="""val a = {123} val c = {"12sadasdfsadf"}""".stripMargin
  println(ASTBuilder.buildAST(program).toString)
}
