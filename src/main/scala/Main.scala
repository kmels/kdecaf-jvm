object Main extends Application{
  val input = io.Source.fromFile("/home/kmels/decaf").mkString
  println("Input: \n"+input+"\n\n")
  import util.parsing.combinator.{lexical,syntactical}
  val result = new compiler.parsing.KDecafParser() parse(input)
  println("AST: \n"+result+"\n\n")
}
