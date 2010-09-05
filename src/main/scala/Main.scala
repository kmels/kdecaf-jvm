package kmels.uvg.kdecaf.main

import kmels.uvg.kdecaf.compiler
import compiler.parsing.{KDecafParser,ast}
import ast.Program
import compiler.semantics._
import compiler.types.aliases.SemanticErrorMessage

object Main extends Application{
  val input = io.Source.fromFile("/home/kmels/tmp/decaf-semantics").mkString

  println("Input: \n"+input+"\n\n")
  import util.parsing.combinator.{lexical,syntactical}
  val result = new KDecafParser() parse(input)
  println("AST: \n"+result+"\n\n")

  import compiler.parsing.ast._
  import compiler.semantics._

  val program:Program = result.get

  program.semanticAction("global") match{
    case SemanticResults(results @ _*) => {
      val errorMessages:List[SemanticErrorMessage] = results.toList.flatMap({ 
	case error:SemanticError => error.errors.toList
	case _ => Nil
      })

      if (errorMessages.size > 0){	
	println(errorMessages.map(errorMessage => "Type Error:"+errorMessage._2.pos.line+":"+errorMessage._1).mkString("\n"))
      }	
      else
	println("well-typed")
    }
    case _ => println("wtf.")
  }
 
  println("Symbol Table: "+kmels.uvg.kdecaf.compiler.SymbolTable.mkString(",\n"))
}
