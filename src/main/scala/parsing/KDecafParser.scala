package parsing

import scala.util.parsing.combinator.{syntactical,lexical}
import syntactical.StandardTokenParsers
import ast._

/**
 * A Parser for the Decaf language
 *
 * @author Carlos Lopez
 * @version 1.0
 * @since 1.0
 */ 

object KDecafParser extends StandardTokenParsers{  
  override val lexical = new KDecafLexer
  
  def program:Parser[Program] = "class" ~> ident ~ declarations ^^ { case programName~declarations => Program(programName,declarations)}

  def declarations:Parser[List[Declaration]] = "{" ~> rep(declaration) <~ "}"

  def declaration:Parser[Declaration] = varDeclaration | structDeclaration // | //methodDeclaration

  def varDeclarations:Parser[List[VarDeclaration]] = "{" ~> rep1(varDeclaration) <~ "}"

  def varDeclaration:Parser[VarDeclaration] = varArrayDeclaration | varType ~ ident <~ ";" ^^ { case varType~id => VarDeclaration(varType,id)} | structVarDeclaration | structConstructorVarDeclaration

  def structConstructorVarDeclaration:Parser[VarDeclaration] = structDeclaration ~ ident <~ ";" ^^ {
      case structDeclaration~id => VarDeclaration(structDeclaration.value,id)
    } | structDeclaration ~ ident ~ arraySizeDeclaration ^^ {
      case structDeclaration~id~arraySie => VarDeclaration(structDeclaration.value,id)
    }
  

  def structVarDeclaration:Parser[VarDeclaration] = "struct" ~> ident ~ ident <~ ";" ^^ {
    case structName ~ id => VarDeclaration(struct(structName),id)
  }
  
  def varArrayDeclaration:Parser[VarDeclaration] = varType ~ ident ~ arraySizeDeclaration ^^ {
    case varType~id~arraySize => 
      VarDeclaration(
	KArray(Array.fill(arraySize)(varType)) //varType has it's value set to default value
	,id
      )      
  }

  def arraySizeDeclaration:Parser[Int] = "[" ~> numericLit <~ "]" <~ ";" ^^ (_.toInt)
  
  def structDeclaration:Parser[StructDeclaration] = "struct" ~> ident ~ varDeclarations ^^ { 
    case structName~varDeclarations => StructDeclaration(structName, Struct(varDeclarations))
  }
 
  def varType:Parser[VarType[_]] = "int" ^^ { _ => int(0) } | "char" ^^ { _ => char(' ')} | "boolean" ^^ {_ => boolean(false)} | "void" ^^ {_ => void({})}

  def parseTokens[T <: lexical.Scanner](tokens:T) = program(tokens)

  def parse(s:String) = {
    val tokens = new lexical.Scanner(s)
    println(tokens)
    parseTokens(tokens)
  }
}
