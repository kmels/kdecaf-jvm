package parsing.ast

/**
 * ASTs class nodes
 *
 * @author Carlos Lopez
 * @version 1.0
 * @since 1.0
 */

sealed trait KDecafAST extends Product{
  override def toString = getClass.getName
  implicit def s(s:String):KDecafAST = StringWrapper(s)
  val children: List[KDecafAST] 
}

case class StringWrapper(val s:String) extends KDecafAST{
  val children = Nil
  override def toString = s
}

case class Program(val name:String, val declarations:List[Declaration]) extends KDecafAST{
  val children = declarations
}

abstract class Declaration extends KDecafAST

case class VarDeclaration(val varType:VarType[_], val id:String) extends Declaration{
  val children = List(s("name: "+id),varType)
}

case class StructDeclaration(val name:String, val value:Struct) extends Declaration{  
  val children = List(s(name),value) 
}

case class MethodDeclaration(val methodType:VarType[_],val name:String,val parameters:List[Parameter],val codeBlock:Block) extends Declaration{
  val children:List[KDecafAST] = List(methodType,s(name))++parameters:+codeBlock
}

trait VarType[+T] extends Expression with KDecafAST{
  val value:T
  override val children:List[KDecafAST] = List(value.toString)
}

abstract class PrimitiveType[+T] extends VarType[T] 

//basic types
case class int(val value:Int) extends PrimitiveType[Int]

case class char(val value:Char) extends PrimitiveType[Char]

case class boolean(val value:Boolean) extends PrimitiveType[Boolean]

case class struct(val value:String) extends VarType[String]{ //the name of the struct 
}

case class void(val value:Unit) extends VarType[Unit]

trait TypeConstructor[+T] extends VarType[T]

case class KArray[U <: VarType[_]](val value:Array[U]) extends TypeConstructor[Array[U]]{
  override val children = value.toList
}

case class Struct(val value:List[VarDeclaration]) extends TypeConstructor[List[VarDeclaration]]{
  override val children = value
}

abstract class Parameter extends KDecafAST{
  val varType:PrimitiveType[_]
  val name:String
  override val children:List[KDecafAST] = List(varType,name)
}

case class PrimitiveTypeParameter(val varType:PrimitiveType[_], val name:String) extends Parameter

case class PrimitiveArrayParameter(val varType:PrimitiveType[_], val name:String) extends Parameter

case class Block(val varDeclarations:List[VarDeclaration], val statements:List[Statement]) extends Statement{
  override val children:List[KDecafAST] = varDeclarations++statements
}

abstract class Statement extends KDecafAST{
  val children:List[KDecafAST]
}

trait ConditionStatement extends Statement{
  val expression:Expression
  val codeBlock:Block
}

case class IfStatement(val expression:Expression,val codeBlock:Block, val elseBlock:Option[Block] = None) extends ConditionStatement{
  val children:List[KDecafAST] = elseBlock match{
    case Some(elseBlock) => List(expression,codeBlock,elseBlock)
    case _ => List(expression,codeBlock)
  }
}

case class WhileStatement(val expression:Expression,val codeBlock:Block) extends ConditionStatement{
  val children:List[KDecafAST] = List(expression,codeBlock)
}

case class MethodCall(val name:String,val arguments:List[Expression]) extends Expression {
  val children:List[KDecafAST] = List(s(name))++arguments
}

case class ReturnStatement(val expression:Option[Expression]) extends Statement{
  val children:List[KDecafAST] = expression match{
    case Some(exp) => List(exp)
    case _ => Nil
  }
}

case class Assignment(val location:Location,val expression:Expression) extends Statement{
  val children:List[KDecafAST] = List(location,expression)
}

abstract class Location extends Expression

case class SimpleLocation(val name:String, val optionalMember:Option[Location] = None) extends Location{
  val children:List[KDecafAST] = optionalMember match{
    case Some(member) => List(s(name),member)
    case _ => List(s(name))
  }
}

case class ArrayLocation(val name:String, val index:Expression, val optionalMember:Option[Location] = None) extends Location{
  val children:List[KDecafAST] = optionalMember match{
    case Some(member) => List(s(name),index,member)
    case _ => List(s(name))
  }
}

abstract class Expression extends Statement

abstract class Operator[T]{
  val lexeme:T
}

case class ArithmeticOperator(val lexeme:Char) extends Operator[Char]

case class InequalityOperator(val lexeme:String) extends Operator[String]

case class EqualityOperator(val lexeme:String) extends Operator[String]

case class ConditionalOperator(val lexeme:String) extends Operator[String]

trait ExpressionOperation extends Expression

trait BinaryOperation[+T] extends ExpressionOperation{
  val exp1:Expression
  val exp2:Expression

  val children:List[Expression] = List(exp1,exp2)
}

trait UnaryOperation extends ExpressionOperation{
  val exp:Expression
  val children:List[Expression] = List(exp)
}

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

