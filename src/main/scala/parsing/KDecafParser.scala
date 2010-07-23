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

  def declaration:Parser[Declaration] = varDeclaration | structDeclaration | methodDeclaration 

  def varDeclarations:Parser[List[VarDeclaration]] = "{" ~> rep(varDeclaration) <~ "}"

  def varDeclaration:Parser[VarDeclaration] = varArrayDeclaration | varType ~ ident <~ ";" ^^ { 
    case varType~id => VarDeclaration(varType,id)
  } | structVarDeclaration | structConstructorVarDeclaration

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
  
  def varType:Parser[VarType[_]] = primitiveType | "void" ^^ {
    _ => void({})
  }

  def primitiveType:Parser[PrimitiveType[_]] = "int" ^^ { _ => int(0) } | "char" ^^ { _ => char(' ')} | "boolean" ^^ {_ => boolean(false)}

  def methodDeclaration:Parser[MethodDeclaration] = varType ~ ident ~ parameterList ~ block ^^ {
    case methodType~name~parameters~codeBlock =>  MethodDeclaration(methodType,name,parameters,codeBlock) 
  }

  def parameterList:Parser[List[Parameter]] = {
    println("parameter list")
    "(" ~> repsep(parameter,",") <~ ")"
  }

  def parameter:Parser[Parameter] = primitiveType ~ ident ^^ {
    case pType~name => PrimitiveTypeParameter(pType,name)
  } | primitiveType ~ ident <~ "[" <~ "]" ^^ {
    case pType~name => PrimitiveArrayParameter(pType,name)
  } 

  def block:Parser[Block] = {
    println("block")
    "{" ~> rep(varDeclaration) ~ statements <~ "}" ^^ {
      case varDeclarations ~ statements => Block(varDeclarations,statements)
    }
  }
  
  def statements:Parser[List[Statement]] =rep(statement)

  def statement:Parser[Statement] =  "_" ^^ { 
    case _ => Statement()
  }

/*  def ifStatement:Parser[IfStatement] 

  def ifElseStatement:Parser[IfElseStatement]

  def whileStatement:Parser[WhileStatement]

  def */
  def parseTokens[T <: lexical.Scanner](tokens:T) = program(tokens)

  def parse(s:String) = {
    val tokens = new lexical.Scanner(s)
    println(tokens)
    parseTokens(tokens)
  }
}
