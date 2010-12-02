package kmels.uvg.kdecaf.main

import kmels.uvg.kdecaf.compiler
import compiler.parsing.{KDecafParser,ast}
import ast.Program
import compiler.semantics._
import compiler.types.aliases.SemanticErrorMessage

object Main extends Application{
  val pathToResourcesDir = System.getProperty("user.dir")+"/src/test/resources/"
  val path = pathToResourcesDir+"fibonacci.kdecaf"
  val input = io.Source.fromFile(path).mkString

  //println("Input: \n"+input+"\n\n")
  import util.parsing.combinator.{lexical,syntactical}
  val result = new KDecafParser() parse(input)
  //println("AST: \n"+result+"\n\n")

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
  
  //println("---------------\n\n\n\n")
  //println("Symbol Table: \n"+kmels.uvg.kdecaf.compiler.SymbolTable.mkString(",\n\t"))
  //println("---------------\n\n\n\n")
  
  if (errorMessages.size > 0){	
    println(errorMessages.map(errorMessage => "Type Error:"+errorMessage._2+": "+errorMessage._1).mkString("\n"))
  }	
  else{
    println("well-typed")

    //println("---------------\n\n\n\n")
    
    val imap = program.imap
    val intermediateCode = imap.foreach(println _)
    
    println("---------------\n\n\n\n")
    
    //println("Fields: \n\t"+kmels.uvg.kdecaf.compiler.CodegenFields.toList.sortBy(_._1._2).mkString(",\n\t"))

    val jmap = program.jmap(imap)
    println("Jasmin code: \n\n\n"+jmap)

    val pathToOutput = pathToResourcesDir+"jasmin-2.4/Program.j"    

    var out_file = new java.io.FileOutputStream(pathToOutput) 
    var out_stream = new java.io.PrintStream(out_file) 
    out_stream.print(jmap) 
    out_stream.close 
    //val file = new java.io.RandomAccessFile(new java.io.File(pathToOutput),"rw")
    //file.writeBytes(jmap)
    println("pathToOutput: "+pathToOutput)
  }    
}
