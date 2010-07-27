package compiler.parsing.ast

/**
 * ASTs class nodes
 *
 * @author Carlos Lopez
 * @version 1.0
 * @since 1.0
 */
sealed trait KDecafAST {
  override def toString = getClass.getName
  implicit def s(s:String):KDecafAST = new StringWrapper(s)
  val children: List[KDecafAST]   

  //string wrapper for listing nodes
  class StringWrapper(val s:String) extends KDecafAST{
    val children = Nil
    override def toString = s
  }
}

case class Program(val name:String, val declarations:List[Declaration]) extends KDecafAST{
  val children = declarations
}

abstract class Declaration extends KDecafAST

case class VarDeclaration(val varType:VarType, val id:String) extends Declaration{
  val children:List[KDecafAST] = List(id,varType)
}

case class StructDeclaration(val name:String, val value:Struct) extends Declaration{  
  val children:List[KDecafAST] = List(name,value) 
}

case class MethodDeclaration(val methodType:VarType,val name:String,val parameters:List[Parameter],val codeBlock:Block) extends Declaration{
  val children:List[KDecafAST] = List[KDecafAST](methodType,name)++parameters:+codeBlock
}

trait VarType extends Expression{
  val children:List[KDecafAST] = Nil
  def getUnderlyingType:String = "None"
}

case class void() extends VarType

case class PrimitiveType[T](implicit m:Manifest[T]) extends VarType{
  override def getUnderlyingType = m.toString

  override def toString = "PrimitiveType["+m.toString+"]"
}

//basic types
case class struct(val value:String) extends VarType //the name of the struct 

trait TypeConstructor[T] extends VarType

case class KArray[U](implicit m:Manifest[U]) extends TypeConstructor[U]{
  override def toString = "KArray["+m.toString+"]"
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
  val children:List[KDecafAST] = List[KDecafAST](name)++arguments
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
    case Some(member) => List(name,member)
    case _ => List(s(name))
  }
}

case class ArrayLocation(val name:String, val index:Expression, val optionalMember:Option[Location] = None) extends Location{
  val children:List[KDecafAST] = optionalMember match{
    case Some(member) => List(name,index,member)
    case _ => List(name)
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

trait ExpressionOperation[T] extends Expression{
  val m:Manifest[T] 
  override def toString = super.toString+": "+m.toString
}

trait BinaryOperation[T] extends ExpressionOperation[T]{
  val exp1:Expression
  val exp2:Expression

  val children:List[Expression] = List(exp1,exp2)
}

trait UnaryOperation[T] extends ExpressionOperation[T]{
  val exp:Expression
  val children:List[Expression] = List(exp)
}

case class ExpressionAdd(val exp1:Expression,val exp2:Expression)(implicit val m:Manifest[Int]) extends BinaryOperation[Int] 

case class ExpressionSub(val exp1:Expression,val exp2:Expression)(implicit val m:Manifest[Int]) extends BinaryOperation[Int] 
case class ExpressionMult(val exp1:Expression, val exp2:Expression)(implicit val m:Manifest[Int]) extends BinaryOperation[Int] 
case class ExpressionDiv(val exp1:Expression, val exp2:Expression)(implicit val m:Manifest[Int]) extends BinaryOperation[Int] 
case class ExpressionMod(val exp1:Expression, val exp2:Expression)(implicit val m:Manifest[Int]) extends BinaryOperation[Int] 

case class ExpressionAnd(val exp1:Expression, val exp2:Expression)(implicit val m:Manifest[Boolean]) extends BinaryOperation[Boolean] 
case class ExpressionOr(val exp1:Expression, val exp2:Expression)(implicit val m:Manifest[Boolean]) extends BinaryOperation[Boolean] 

case class ExpressionLessOrEquals(val exp1:Expression, val exp2:Expression)(implicit val m:Manifest[Boolean]) extends BinaryOperation[Boolean] 
case class ExpressionLess(val exp1:Expression, val exp2:Expression)(implicit val m:Manifest[Boolean]) extends BinaryOperation[Boolean] 
case class ExpressionGreater(val exp1:Expression, val exp2:Expression)(implicit val m:Manifest[Boolean]) extends BinaryOperation[Boolean] 
case class ExpressionGreaterOrEquals(val exp1:Expression, val exp2:Expression)(implicit val m:Manifest[Boolean]) extends BinaryOperation[Boolean] 
case class ExpressionEquals(val exp1:Expression, val exp2:Expression)(implicit val m:Manifest[Boolean]) extends BinaryOperation[Boolean] 
case class ExpressionNotEquals(val exp1:Expression, val exp2:Expression)(implicit val m:Manifest[Boolean]) extends BinaryOperation[Boolean] 


case class NegativeExpression(val exp:Expression)(implicit val m:Manifest[Int]) extends UnaryOperation[Int] 
case class NotExpression(val exp:Expression)(implicit val m:Manifest[Boolean]) extends UnaryOperation[Boolean] 

trait Literal[T] extends Expression{
  val m:Manifest[T]
  val literal:T
  override val children = Nil
  override def toString = "Literal["+m+"]"
}

case class IntLiteral(val literal:Int)(implicit val m:Manifest[Int]) extends Literal[Int]()

case class CharLiteral(val literal:Char)(implicit val m:Manifest[Char]) extends Literal[Char]()

case class BoolLiteral(val literal:Boolean)(implicit val m:Manifest[Boolean]) extends Literal[Boolean]()
