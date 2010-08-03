object Main extends Application{
  val input = io.Source.fromFile("/home/kmels/decaf").mkString
  println("Input: \n"+input+"\n\n")
  import util.parsing.combinator.{lexical,syntactical}
  val result = new compiler.parsing.KDecafParser() parse(input)
  println("AST: \n"+result+"\n\n")

  import compiler.parsing.ast._
  import compiler.semantics._

  val program:Program = result.get

  try {    
    program.semanticAction(compiler.semantics.SemanticAttributes(Some("global")))
  } catch{
    case e => println(e.toString)
  }

  /*.foreach{
    case SemanticAction(action) => try {
      action()
    } catch{
      case e:SemanticError => println(e.toString)
    }
  }*/

  println("Symbol Table: "+compiler.SymbolTable.mkString(",\n"))
}
