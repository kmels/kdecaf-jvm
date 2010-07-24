package parsing

import lexical.KDecafLexer
import scala.util.parsing.combinator.{syntactical}
import syntactical.{StandardTokenParsers}
import scala.util.parsing.input.CharArrayReader.EofCh
import ast._

/**
 * A Parser for the Decaf language
 *
 * @author Carlos Lopez
 * @version 1.0
 * @since 1.0
 */ 

class KDecafParser extends StandardTokenParsers{  
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

  def block:Parser[Block] = "{" ~> rep(varDeclaration) ~ statements <~ "}" ^^ {
    case varDeclarations ~ statements => Block(varDeclarations,statements)
  }
  
  def statements:Parser[List[Statement]] =rep(statement)

  def statement:Parser[Statement] = ifStatement | whileStatement | returnStatement | methodCall <~ ";" | block | assignment | expression <~ ";"

  def parenthesisExpression:Parser[Expression] = "(" ~> expression <~ ")"

  def arguments:Parser[List[Expression]] = "(" ~> repsep(expression,",") <~ ")"

  def ifStatement:Parser[IfStatement] = ifElseStatement | "if" ~> parenthesisExpression ~ block ^^ {
    case expr ~ ifBlock => IfStatement(expr,ifBlock)
  }

  def ifElseStatement:Parser[IfStatement] = "if" ~> parenthesisExpression ~ block ~ "else" ~ block ^^ {
    case expr ~ ifBlock ~ "else" ~ elseBlock => IfStatement(expr,ifBlock,Some(elseBlock))
  }

  def whileStatement:Parser[WhileStatement] = "while" ~> parenthesisExpression ~ block ^^ {
    case expression_ ~ block => WhileStatement(expression_,block)
  }

  def returnStatement:Parser[ReturnStatement] = "return" ~> opt(expression) <~ ";" ^^ { ReturnStatement(_) }

  def methodCall:Parser[MethodCall] = ident ~ arguments ^^ {
    case id~args => MethodCall(id,args)
  }

  def assignment:Parser[Assignment] = location ~ "=" ~ expression ^^ {
    case loc ~ "=" ~ expr => Assignment(loc,expr)
  }

  def expression:Parser[Expression] =  methodCall | location | literal

  def literal:Parser[Literal[_]] = numericLit ^^ { IntLiteral(_)} | charLit | "true" ^^ { BoolLiteral(_)} | "false" ^^ { BoolLiteral(_)}

  def charLit:Parser[CharLiteral] = elem("char", x => { x.isInstanceOf[lexical.CharLit] }) ^^ {c => CharLiteral(c.chars)}
 
  def location:Parser[Location] = arrayLocation | ident ~ optionalLocation ^^ { 
    case id~optLocation => SimpleLocation(id,optLocation)
  } 

  def optionalLocation:Parser[Option[Location]] = opt("." ~> location)

  def arrayLocation:Parser[Location] = ident ~ arrayLocationExpression ~ optionalLocation ^^ {
    case id~exp~optLocation => ArrayLocation(id,exp,optLocation)
  }
 
  def arrayLocationExpression:Parser[Expression] = "[" ~> expression <~ "]"

  def parseTokens[T <: lexical.Scanner](tokens:T) = program(tokens)

  def parse(s:String) = {
    val tokens = new lexical.Scanner(s)
    println(tokens)
    parseTokens(tokens)
  }
}
