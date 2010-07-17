package parsing

import scala.util.parsing.combinator.{syntactical,lexical}
import syntactical.StandardTokenParsers
import ast._

object KDecafParser extends StandardTokenParsers{  
  override val lexical = new KDecafLexer
  
  def program = "class" ~> ident ~ declarations ^^ { case programName~declarations => Program(programName,declarations)}

  def declarations:Parser[List[Declaration]] = "{" ~> rep(declaration) <~ "}"

  def declaration:Parser[Declaration] = varDeclaration// | //methodDeclaration

  def varDeclarations:Parser[List[VarDeclaration]] = "{" ~> rep1(varDeclaration) <~ "}"

  def varDeclaration:Parser[VarDeclaration] = varArrayDeclaration | varType ~ ident <~ ";" ^^ { case varType~id => VarDeclaration(varType,id)} 

  def varArrayDeclaration:Parser[VarDeclaration] = varType ~ ident ~ arraySizeDeclaration  ^^ { case varType~id~arraySize => VarDeclaration(ArrayOf(varType,arraySize),id)}

  def arraySizeDeclaration:Parser[Int] = "[" ~> numericLit <~ "]" <~ ";" ^^ (_.toInt)

  def structDeclaration:Parser[StructDeclaration] = "struct" ~> ident ~ varDeclarations ^^ { case structName~varDeclarations => StructDeclaration(structName, struct(varDeclarations))}
 
  def varType:Parser[VarType] = "int" ^^ { _ => int() } | "char" ^^ { _ => char()} | "boolean" ^^ {_ => boolean()} | "void" ^^ {_ => void()} //| structDeclaration 

  def parseTokens[T <: lexical.Scanner](tokens:T) = program(tokens)

  def parse(s:String) = {
    val tokens = new lexical.Scanner(s)
    println(tokens)
    parseTokens(tokens)
  }
}
