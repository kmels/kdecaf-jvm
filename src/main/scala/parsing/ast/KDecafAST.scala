package parsing.ast

/**
 * ASTs class nodes
 *
 * @author Carlos Lopez
 * @version 1.0
 * @since 1.0
 */

sealed class KDecafAST

case class Program(val name:String, val declarations:List[Declaration]) extends KDecafAST{
  override def toString = "Program("+name+","+declarations.mkString("\n")+")"
}

abstract class Declaration extends KDecafAST

case class VarDeclaration(val varType:VarType[_], val id:String) extends Declaration

case class StructDeclaration(val name:String, val value:Struct) extends Declaration

case class MethodDeclaration(val methodType:VarType[_],val name:String,val parameters:List[Parameter],val codeBlock:Block) extends Declaration

trait VarType[+T] extends Expression{
  val value:T
}

abstract class PrimitiveType[+T] extends VarType[T] 

//basic types
case class int(val value:Int) extends PrimitiveType[Int]

case class char(val value:Char) extends PrimitiveType[Char]

case class boolean(val value:Boolean) extends PrimitiveType[Boolean]

case class struct(val value:String) extends VarType[String] //the name of the struct

case class void(val value:Unit) extends VarType[Unit]

trait TypeConstructor[+T] extends VarType[T]

case class KArray[U <: VarType[_]](val value:Array[U]) extends TypeConstructor[Array[U]]

case class Struct(val value:List[VarDeclaration]) extends TypeConstructor[List[VarDeclaration]]

abstract class Parameter 

case class PrimitiveTypeParameter(val varType:PrimitiveType[_], val name:String) extends Parameter

case class PrimitiveArrayParameter(val varType:PrimitiveType[_], val name:String) extends Parameter

case class Block(val varDeclarations:List[VarDeclaration], val statements:List[Statement]) extends Statement

abstract class Statement

trait ConditionStatement extends Statement{
  val expression:Expression
  val codeBlock:Block
}

case class IfStatement(val expression:Expression,val codeBlock:Block, val elseBlock:Option[Block] = None) extends ConditionStatement

case class WhileStatement(val expression:Expression,val codeBlock:Block) extends ConditionStatement

case class MethodCall(val name:String,val arguments:List[Expression]) extends Expression 

case class ReturnStatement(val expression:Option[Expression]) extends Statement

case class Assignment(val location:Location,val expression:Expression) extends Statement

abstract class Location extends Expression

case class SimpleLocation(val name:String, val optionalMember:Option[Location] = None) extends Location

case class ArrayLocation(val name:String, val index:Expression, val optionalMember:Option[Location] = None) extends Location

/*abstract class Literal[+T] extends Expression{
  val literal:String
  val value: T
} 

case class IntLiteral(val literal:String) extends Literal[Int]{
  val value = literal.toInt
}
case class CharLiteral(val literal:String) extends Literal[Char]{
  val value = literal.charAt(0)
}
case class BoolLiteral(val literal:String) extends Literal[Boolean]{
  val value = literal.toBoolean
}*/

abstract class Expression extends Statement

abstract class Operator[T]{
  val lexeme:T
}

case class ArithmeticOperator(val lexeme:Char) extends Operator[Char]

case class InequalityOperator(val lexeme:String) extends Operator[String]

case class EqualityOperator(val lexeme:String) extends Operator[String]

case class ConditionalOperator(val lexeme:String) extends Operator[String]

trait ExpressionOperation extends Expression

trait BinaryOperation[+T] extends ExpressionOperation

trait UnaryOperation extends ExpressionOperation

case class ExpressionAdd(val exp1:Expression,val exp2:Expression) extends BinaryOperation[Int]
case class ExpressionSub(val exp1:Expression,val exp2:Expression) extends BinaryOperation[Int]
case class ExpressionMult(val exp1:Expression, val exp2:Expression) extends BinaryOperation[Int]
case class ExpressionDiv(val exp1:Expression, val exp2:Expression) extends BinaryOperation[Int]
case class ExpressionMod(val exp1:Expression, val exp2:Expression) extends BinaryOperation[Int]

case class ExpressionAnd(val exp1:Expression, val exp2:Expression) extends BinaryOperation[Boolean]
case class ExpressionOr(val exp1:Expression, val exp2:Expression) extends BinaryOperation[Boolean]

case class ExpressionLessOrEquals(val exp1:Expression, val exp2:Expression) extends BinaryOperation[Boolean]
case class ExpressionLess(val exp1:Expression, val exp2:Expression) extends BinaryOperation[Boolean]
case class ExpressionGreater(val exp1:Expression, val exp2:Expression) extends BinaryOperation[Boolean]
case class ExpressionGreaterOrEquals(val exp1:Expression, val exp2:Expression) extends BinaryOperation[Boolean]
case class ExpressionEquals(val exp1:Expression, val exp2:Expression) extends BinaryOperation[Boolean]
case class ExpressionNotEquals(val exp1:Expression, val exp2:Expression) extends BinaryOperation[Boolean]


case class NegativeExpression(val exp:Expression) extends UnaryOperation
case class NotExpression(val exp:Expression) extends UnaryOperation

