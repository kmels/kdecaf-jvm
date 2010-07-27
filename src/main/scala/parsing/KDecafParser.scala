package parsing

import scala.util.parsing.combinator.{syntactical,PackratParsers}
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
class KDecafParser extends StandardTokenParsers with PackratParsers{  
  override val lexical = new KDecafLexer
  
  lazy val program:PackratParser[Program] = "class" ~> ident ~ declarations ^^ { case programName~declarations => Program(programName,declarations)}

  lazy val declarations:PackratParser[List[Declaration]] = "{" ~> rep(declaration) <~ "}"

  lazy val declaration:PackratParser[Declaration] = varDeclaration | structDeclaration | methodDeclaration 

  lazy val varDeclarations:PackratParser[List[VarDeclaration]] = "{" ~> rep(varDeclaration) <~ "}"

  lazy val varDeclaration:PackratParser[VarDeclaration] = varArrayDeclaration | varType ~ ident <~ ";" ^^ { 
    case varType~id => VarDeclaration(varType,id)
  } | structVarDeclaration | structConstructorVarDeclaration

  lazy val structConstructorVarDeclaration:PackratParser[VarDeclaration] = structDeclaration ~ ident <~ ";" ^^ {
    case structDeclaration~id => VarDeclaration(structDeclaration.value,id)
  } | structDeclaration ~ ident ~ arraySizeDeclaration ^^ {
    case structDeclaration~id~arraySie => VarDeclaration(structDeclaration.value,id)
  } //OJO AKI! 
  
  lazy val structVarDeclaration:PackratParser[VarDeclaration] = "struct" ~> ident ~ ident <~ ";" ^^ {
    case structName ~ id => VarDeclaration(struct(structName),id)
  }
  
  lazy val varArrayDeclaration:PackratParser[VarDeclaration] = varType ~ ident ~ arraySizeDeclaration ^^ {
    case varType~id~arraySize => {
      val array = varType.getUnderlyingType match{
	case "None" => KArray()
	case "Int" => KArray[Int]()
	case "Char" => KArray[Char]()
	case "Boolean" => KArray[Boolean]()
      }
      VarDeclaration(array,id)  
    }
  }

  lazy val arraySizeDeclaration:PackratParser[Int] = "[" ~> numericLit <~ "]" <~ ";" ^^ (_.toInt)
  
  lazy val structDeclaration:PackratParser[StructDeclaration] = "struct" ~> ident ~ varDeclarations ^^ { 
    case structName~varDeclarations => StructDeclaration(structName, Struct(varDeclarations))
  }
  
  lazy val varType:PackratParser[VarType] = primitiveType | "void" ^^ {
    _ => void()
  }

  lazy val primitiveType:PackratParser[PrimitiveType[_]] =  primitiveChar | primitiveIntOrBool

  lazy val primitiveIntOrBool:PackratParser[PrimitiveType[_]] = "int" ^^ { _ => PrimitiveType[Int]() } | "boolean" ^^ {_ => PrimitiveType[Boolean]()}
   
  lazy val primitiveChar:PackratParser[PrimitiveType[Char]] = "char" ^^ { _ => PrimitiveType[Char]()} 
  

  lazy val methodDeclaration:PackratParser[MethodDeclaration] = varType ~ ident ~ parameterList ~ block ^^ {
    case methodType~name~parameters~codeBlock =>  MethodDeclaration(methodType,name,parameters,codeBlock) 
  }

  lazy val parameterList:PackratParser[List[Parameter]] = {
    "(" ~> repsep(parameter,",") <~ ")"
  }

  lazy val parameter:PackratParser[Parameter] = primitiveType ~ ident ^^ {
    case pType~name => PrimitiveTypeParameter(pType,name)
  } | primitiveType ~ ident <~ "[" <~ "]" ^^ {
    case pType~name => PrimitiveArrayParameter(pType,name)
  } 

  lazy val block:PackratParser[Block] = "{" ~> rep(varDeclaration) ~ statements <~ "}" ^^ {
    case varDeclarations ~ statements => Block(varDeclarations,statements)
  }
  
  lazy val statements:PackratParser[List[Statement]] =rep(statement)

  lazy val statement:PackratParser[Statement] = ifStatement | whileStatement | returnStatement | methodCall <~ ";" | block | assignment | expression <~ ";"

  lazy val parenthesisExpression:PackratParser[Expression] = "(" ~> expression <~ ")"

  lazy val arguments:PackratParser[List[Expression]] = "(" ~> repsep(expression,",") <~ ")"

  lazy val ifStatement:PackratParser[IfStatement] = ifElseStatement | "if" ~> parenthesisExpression ~ block ^^ {
    case expr ~ ifBlock => IfStatement(expr,ifBlock)
  }

  lazy val ifElseStatement:PackratParser[IfStatement] = "if" ~> parenthesisExpression ~ block ~ "else" ~ block ^^ {
    case expr ~ ifBlock ~ "else" ~ elseBlock => IfStatement(expr,ifBlock,Some(elseBlock))
  }

  lazy val whileStatement:PackratParser[WhileStatement] = "while" ~> parenthesisExpression ~ block ^^ {
    case expression_ ~ block => WhileStatement(expression_,block)
  }

  lazy val returnStatement:PackratParser[ReturnStatement] = "return" ~> opt(expression) <~ ";" ^^ { ReturnStatement(_) }

  lazy val methodCall:PackratParser[MethodCall] = ident ~ arguments ^^ {
    case id~args => MethodCall(id,args)
  }

  lazy val assignment:PackratParser[Assignment] = location ~ "=" ~ expression ^^ {
    case loc ~ "=" ~ expr => Assignment(loc,expr)
  }

  lazy val expression:PackratParser[Expression] = expressionOperation ~ opt(
    ("&&"|"||") ~ expression 
  ) ^^ {
    case exp ~ None => exp
    case exp1 ~ Some(expressionWithOperator) => expressionWithOperator match{
      case "&&" ~ exp2 => ExpressionAnd(exp1,exp2)
      case "||" ~ exp2 => ExpressionOr(exp1,exp2)
    }
  }

  lazy val expressionOperation:PackratParser[Expression] = expressionSum ~ opt(
    ("<="|"<"|">"|">="|"=="|"!=") ~ expressionSum
  ) ^^ {
    case exp ~ None => exp
    case exp1 ~ Some(comparedExpression) => comparedExpression match{
      case compareSymbol ~ exp2 => compareSymbol match{
	case "<=" => ExpressionLessOrEquals(exp1,exp2)
	case "<" => ExpressionLess(exp1,exp2)
	case ">" => ExpressionGreater(exp1,exp2)
	case ">=" => ExpressionGreaterOrEquals(exp1,exp2)
	case "==" => ExpressionEquals(exp1,exp2)
	case "!=" => ExpressionNotEquals(exp1,exp2)
      }
    }
  }

  lazy val expressionSum:PackratParser[Expression] = expressionMult ~ opt( 
    ("+"|"-") ~ expressionSum 
  ) ^^ {
    case exp ~ None => exp
    case exp1 ~ Some(exp2WithOperator) => exp2WithOperator match{
      case "+" ~ exp2 => ExpressionAdd(exp1,exp2)
      case "-" ~ exp2 => ExpressionSub(exp1,exp2)
    } 
  }

  lazy val expressionMult:PackratParser[Expression] = unaryOperationExpression ~ opt( 
    ("*"|"/"|"%") ~ expressionMult
  ) ^^ {
    case exp ~ None => exp
    case exp1 ~ Some(exp2WithOperator) => exp2WithOperator match{
      case "*" ~ exp2 => ExpressionMult(exp1,exp2)
      case "%" ~ exp2 => ExpressionMod(exp1,exp2)
      case "/" ~ exp2 => ExpressionDiv(exp1,exp2)
    }
  }
   
  lazy val unaryOperationExpression :PackratParser[Expression] = "-" ~> simpleExpression ^^ { NegativeExpression(_)} | "!" ~> simpleExpression ^^ { NotExpression(_)} | simpleExpression

  //expression without operations
  lazy val simpleExpression:PackratParser[Expression] = literal | "(" ~> expression <~ ")" | methodCall  | location

  lazy val literal:PackratParser[Literal[_]] = numericLit ^^ { lit => IntLiteral(lit.toInt)} |  boolOrCharLit

  lazy val boolOrCharLit:PackratParser[Literal[_]] = boolLit | charLit

  lazy val boolLit:PackratParser[BoolLiteral] = ("true"|"false") ^^ { case v => BoolLiteral(v.toBoolean)}

  lazy val charLit:PackratParser[CharLiteral] = elem("char", x => { x.isInstanceOf[lexical.CharLit] }) ^^ {c => CharLiteral(c.chars.charAt(0))}
 
  lazy val location:PackratParser[Location] = arrayLocation | ident ~ optionalLocation ^^ { 
    case id~optLocation => SimpleLocation(id,optLocation)
  } 

  lazy val optionalLocation:PackratParser[Option[Location]] = opt("." ~> location)

  lazy val arrayLocation:PackratParser[Location] = ident ~ arrayLocationExpression ~ optionalLocation ^^ {
    case id~exp~optLocation => ArrayLocation(id,exp,optLocation)
  }
 
  lazy val arrayLocationExpression:PackratParser[Expression] = "[" ~> expression <~ "]"

  def parseTokens[T <: lexical.Scanner](tokens:T) = program(tokens)

  def parse(s:String) = {
    val tokens = new lexical.Scanner(s)
    val packratReader = new PackratReader(tokens)
    program(packratReader)
  }
}
