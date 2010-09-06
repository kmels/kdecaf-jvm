package kmels.uvg.kdecaf.main

import kmels.uvg.kdecaf.compiler
import compiler.parsing.{KDecafParser,ast}
import ast.Program
import compiler.semantics._
import compiler.types.aliases.SemanticErrorMessage

object Main extends Application{
  val path = System.getProperty("user.dir")+"/src/test/resources/semantics.decaf"
  val input = io.Source.fromFile(path).mkString

  println("Input: \n"+input+"\n\n")
  import util.parsing.combinator.{lexical,syntactical}
  val result = new KDecafParser() parse(input)
  println("AST: \n"+result+"\n\n")

  import compiler.parsing.ast._
  import compiler.semantics._
  
  val program:Program = result.get
  
  def getErrorMessages(result:SemanticResult):List[SemanticErrorMessage] = result match{
    case SemanticResults(results @ _*) => results.toList.flatMap{
      case error:SemanticError => error.errors.toList
      case semanticResults:SemanticResults => getErrorMessages(semanticResults)
      case SemanticSuccess => Nil
    }
  }
 
  val errorMessages:List[SemanticErrorMessage] = getErrorMessages(program.semanticAction("global"))
    
  if (errorMessages.size > 0){	
    println(errorMessages.map(errorMessage => "Type Error:"+errorMessage._2+": "+errorMessage._1).mkString("\n"))
  }	
  else
    println("well-typed")
 
  println("Symbol Table: "+kmels.uvg.kdecaf.compiler.SymbolTable.mkString(",\n"))
}
