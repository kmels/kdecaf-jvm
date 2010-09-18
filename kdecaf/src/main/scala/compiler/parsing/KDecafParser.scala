package kmels.uvg.kdecaf.compiler.parsing

import scala.util.parsing.combinator.{syntactical,PackratParsers}
import syntactical.{StandardTokenParsers}
import scala.util.parsing.input.CharArrayReader.EofCh
import kmels.uvg.kdecaf.compiler
import compiler.SymbolTable
import compiler.parsing.ast._

/**
 * A Parser for the Decaf language
 *
 * @author Carlos Lopez
 * @version 1.0
 * @since 1.0
 */ 
class KDecafParser extends StandardTokenParsers with PackratParsers{  
  override val lexical = new KDecafLexer
  
  lazy val program:PackratParser[Program] = positioned("class" ~> ident ~ declarations ^^ { case programName~declarations => Program(programName,declarations)})

  lazy val declarations:PackratParser[List[Declaration]] = "{" ~> rep(declaration) <~ "}"

  lazy val declaration:PackratParser[Declaration] = varDeclaration | structDeclaration | methodDeclaration

  lazy val varDeclarations:PackratParser[List[VarDeclaration]] = "{" ~> rep(varDeclaration) <~ "}"

  lazy val varDeclaration:PackratParser[VarDeclaration] = positioned(varArrayDeclaration | varType ~ ident <~ ";" ^^ { 
    case varType~id => VarDeclaration(varType,id)
  } | structVarDeclaration | structConstructorVarDeclaration)

  lazy val structConstructorVarDeclaration:PackratParser[VarDeclaration] = positioned(structDeclaration ~ ident <~ ";" ^^ {
    case structDeclaration~id => VarDeclaration(structDeclaration.value,id)
  } | structDeclaration ~ ident ~ arraySizeDeclaration ^^ {
    case structDeclaration~id~arraySie => VarDeclaration(structDeclaration.value,id)
  }) //OJO AKI! 
  
  lazy val structVarDeclaration:PackratParser[VarDeclaration] = positioned("struct" ~> ident ~ ident <~ ";" ^^ {
    case structName ~ id => VarDeclaration(struct(structName),id)
  })
  
  lazy val varArrayDeclaration:PackratParser[VarDeclaration] = positioned(varType ~ ident ~ arraySizeDeclaration ^^ {
    case varType~id~arraySize => {
      val array:TypeConstructor[AnyVal] = varType.getUnderlyingType() match{
	case "None" => KArray(arraySize)
	case "Int" => KArray[Int](arraySize)
	case "Char" => KArray[Char](arraySize)
	case "Boolean" => KArray[Boolean](arraySize)
      }
      VarDeclaration(array,id)
    }
  })

  lazy val arraySizeDeclaration:PackratParser[Int] = "[" ~> numericLit <~ "]" <~ ";" ^^ (_.toInt)
  
  lazy val structDeclaration:PackratParser[StructDeclaration] = positioned("struct" ~> ident ~ varDeclarations ^^ { 
    case structName~varDeclarations => StructDeclaration(structName, Struct(varDeclarations))
  })
  
  lazy val varType:PackratParser[VarType] = primitiveType | "void" ^^ { _ => void }

  lazy val primitiveType:PackratParser[PrimitiveType[AnyVal]] = positioned("int" ^^ { _ => PrimitiveInt } | "boolean" ^^ {_ => PrimitiveBoolean} | "char" ^^ { _ => PrimitiveChar})

  lazy val methodDeclaration:PackratParser[MethodDeclaration] = positioned(varType ~ ident ~ parameterList ~ block ^^ {
    case methodType~name~parameters~codeBlock =>  MethodDeclaration(methodType,name,parameters,codeBlock) 
  })

  lazy val parameterList:PackratParser[List[Parameter]] = {
    "(" ~> repsep(parameter,",") <~ ")"
  }

  lazy val parameter:PackratParser[Parameter] = positioned(primitiveType ~ ident ^^ {
    case pType~name => PrimitiveTypeParameter(pType,name)
  } | primitiveType ~ ident <~ "[" <~ "]" ^^ {
    case pType~name => PrimitiveArrayParameter(pType,name)
  })

  lazy val block:PackratParser[Block] = positioned("{" ~> rep(varDeclaration) ~ statements <~ "}" ^^ {
    case varDeclarations ~ statements => Block(varDeclarations,statements)
  })
  
  lazy val statements:PackratParser[List[Statement]] =rep(statement)

  lazy val statement:PackratParser[Statement] = positioned(ifStatement | whileStatement | returnStatement | methodCall <~ ";" | block | assignment | expressionStatement)

  lazy val expressionStatement:PackratParser[Expression] = expression <~ ";" | ";" ^^ {case ";" => EmptyExpression}

  lazy val parenthesisExpression:PackratParser[Expression] = "(" ~> expression <~ ")"

  lazy val arguments:PackratParser[List[Expression]] = "(" ~> repsep(expression,",") <~ ")"

  lazy val ifStatement:PackratParser[IfStatement] = positioned(ifElseStatement | "if" ~> parenthesisExpression ~ block ^^ {
    case expr ~ ifBlock => IfStatement(expr,ifBlock)
  })

  lazy val ifElseStatement:PackratParser[IfStatement] = positioned("if" ~> parenthesisExpression ~ block ~ "else" ~ block ^^ {
    case expr ~ ifBlock ~ "else" ~ elseBlock => IfStatement(expr,ifBlock,Some(elseBlock))
  })

  lazy val whileStatement:PackratParser[WhileStatement] = positioned("while" ~> parenthesisExpression ~ block ^^ {
    case expression_ ~ block => WhileStatement(expression_,block)
  })

  lazy val returnStatement:PackratParser[ReturnStatement] = positioned("return" ~> opt(expression) <~ ";" ^^ { ReturnStatement(_) })

  lazy val methodCall:PackratParser[MethodCall] = positioned(ident ~ arguments ^^ {
    case id~args => MethodCall(id,args)
  })

  lazy val assignment:PackratParser[Assignment] = positioned(location ~ "=" ~ expression ^^ {
    case loc ~ "=" ~ expr => Assignment(loc,expr)
  })

  lazy val expression:PackratParser[Expression] = positioned(expressionOperation ~ opt(
    ("&&"|"||") ~ expression 
  ) ^^ {
    case exp ~ None => exp
    case exp1 ~ Some(expressionWithOperator) => expressionWithOperator match{
      case "&&" ~ exp2 => ExpressionAnd(exp1,exp2)
      case "||" ~ exp2 => ExpressionOr(exp1,exp2)
    }
  })

  lazy val expressionOperation:PackratParser[Expression] = positioned(expressionSum ~ opt(
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
  })

  lazy val expressionSum:PackratParser[Expression] = positioned(expressionMult ~ opt( 
    ("+"|"-") ~ expressionSum 
  ) ^^ {
    case exp ~ None => exp
    case exp1 ~ Some(exp2WithOperator) => exp2WithOperator match{
      case "+" ~ exp2 => ExpressionAdd(exp1,exp2)
      case "-" ~ exp2 => ExpressionSub(exp1,exp2)
    } 
  })

  lazy val expressionMult:PackratParser[Expression] = positioned(unaryOperationExpression ~ opt( 
    ("*"|"/"|"%") ~ expressionMult
  ) ^^ {
    case exp ~ None => exp
    case exp1 ~ Some(exp2WithOperator) => exp2WithOperator match{
      case "*" ~ exp2 => ExpressionMult(exp1,exp2)
      case "%" ~ exp2 => ExpressionMod(exp1,exp2)
      case "/" ~ exp2 => ExpressionDiv(exp1,exp2)
    }
  })
   
  lazy val unaryOperationExpression :PackratParser[Expression] = positioned("-" ~> simpleExpression ^^ { NegativeExpression(_)} | "!" ~> simpleExpression ^^ { NotExpression(_)} | simpleExpression)

  //expression without operations
  lazy val simpleExpression:PackratParser[Expression] = positioned(literal | "(" ~> expression <~ ")" | methodCall  | location)

  lazy val literal:PackratParser[Literal[AnyVal]] = 
    positioned(numericLit ^^ { lit => IntLiteral(lit.toInt)} | ("true"|"false") ^^ { case v => BoolLiteral(v.toBoolean)} | elem("char", x => { x.isInstanceOf[lexical.CharLit] }) ^^ {c => CharLiteral(c.chars.charAt(0))})
 
  lazy val location:PackratParser[Location] = positioned(arrayLocation | ident ~ optionalLocation ^^ { 
    case id~optLocation => SimpleLocation(id,optLocation)
  })

  lazy val optionalLocation:PackratParser[Option[Location]] = opt("." ~> location)

  lazy val arrayLocation:PackratParser[Location] = positioned(ident ~ arrayLocationExpression ~ optionalLocation ^^ {
    case id~exp~optLocation => ArrayLocation(id,exp,optLocation)
  })
 
  lazy val arrayLocationExpression:PackratParser[Expression] = "[" ~> expression <~ "]"

  def parseTokens[T <: lexical.Scanner](tokens:T) = program(tokens)

  def parse(s:String) = {
    val tokens = new lexical.Scanner(s)
    val packratReader = new PackratReader(tokens)
    program(packratReader)
  }
}
