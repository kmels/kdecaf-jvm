package kmels.uvg.kdecaf.main

import kmels.uvg.kdecaf.compiler.parsing.KDecafParser
import kmels.uvg.kdecaf.compiler.parsing.ast.Program

object Main extends Application{
  val input = io.Source.fromFile("/home/kmels/tmp/decaf-semantics").mkString

  println("Input: \n"+input+"\n\n")
  import util.parsing.combinator.{lexical,syntactical}
  val result = new KDecafParser() parse(input)
  println("AST: \n"+result+"\n\n")

  import compiler.parsing.ast._
  import compiler.semantics._

  val program:Program = result.get

  println("MMMMMM!")
  try {    
    println("No semantics!")
    program.semanticAction(kmels.uvg.kdecaf.compiler.semantics.SemanticAttributes(Some("global")))
  } catch{
    case e => println("Semantic Error: "+e.toString)
  }

  /*.foreach{
    case SemanticAction(action) => try {
      action()
    } catch{
      case e:SemanticError => println(e.toString)
    }
  }*/

  println("Symbol Table: "+kmels.uvg.kdecaf.compiler.SymbolTable.mkString(",\n"))
}
