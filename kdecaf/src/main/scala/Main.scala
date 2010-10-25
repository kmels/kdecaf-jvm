package kmels.uvg.kdecaf.main

import kmels.uvg.kdecaf.compiler
import compiler.parsing.{KDecafParser,ast}
import ast.Program
import compiler.semantics._
import compiler.types.aliases.SemanticErrorMessage

object Main extends Application{
  val path = System.getProperty("user.dir")+"/src/test/resources/codegen.decaf"
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
 
//  println("Program semantic results: "+program.semanticAction("Program"))

  val errorMessages:List[SemanticErrorMessage] = getErrorMessages(program.semanticAction("Program"))
  
  println("---------------\n\n\n\n")
  println("Symbol Table: \n"+kmels.uvg.kdecaf.compiler.SymbolTable.mkString(",\n\t"))
  println("---------------\n\n\n\n")
  
  if (errorMessages.size > 0){	
    println(errorMessages.map(errorMessage => "Type Error:"+errorMessage._2+": "+errorMessage._1).mkString("\n"))
  }	
  else{
    println("well-typed")  

    println("---------------\n\n\n\n")
    
    val intermediateCode = program.imap.foreach(println _)
    
    println("---------------\n\n\n\n")
    
    println("Fields: \n\t"+kmels.uvg.kdecaf.compiler.CodegenFields.toList.sortBy(_._1._2).mkString(",\n\t"))
  }    
}
