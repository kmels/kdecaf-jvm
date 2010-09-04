package kmels.uvg.kdecaf.compiler.parsing.ast

import kmels.uvg.kdecaf.compiler._
import semantics._
import kmels.uvg.kdecaf.compiler.types.{aliases => typeAliases,AttributeList}
import typeAliases._
import scala.util.parsing.input.{Positional,Position}

//import compiler.SymbolTable
//import compiler.semantics._

/**
 * ASTs class nodes
 *
 * @author Carlos Lopez
 * @version 2.0
 * @since 1.0
 */
trait Node extends Positional{
  override def toString = getClass.getName
  implicit def s(s:String):Node = new StringWrapper(s)

  val children: List[Node]   

  //string wrapper for listing nodes
  class StringWrapper(val s:String) extends Node{
    val children = Nil
    override def toString = s
  }
}

trait InnerType {
  val getUnderlyingType: () => String
}

trait InnerInt extends InnerType{
  val getUnderlyingType = () => "Int"
}

trait InnerBool extends InnerType{
  val getUnderlyingType = () => "Boolean"
}

trait InnerChar extends InnerType{
  val getUnderlyingType = () => "Char"
}

trait NoInnerType extends InnerType{
  val getUnderlyingType = () => "Nothing"
}

case class Program(val name:String, val declarations:List[Declaration]) extends Node with SemanticRule{ 
  val children = declarations

  val semanticAction = SemanticAction(
    (attributes: SemanticAttributes) => {
      val semanticAttributes = SemanticAttributes(Some(name)) //attributes with the name of the scope
      declarations.foreach(d => d.semanticAction(semanticAttributes))
    }
  )
}

abstract class Declaration extends Node with SemanticRule{
  val name:String
  
  // don't allow duplicate names of variables
  val validateDuplicates = (attributes: SemanticAttributes) => {      
    //val scope = attributesToScope(attributes)
    if (SymbolTable.contains((name,attributes)))
      SemanticError(name+"\" already declared in scope \"global\"")
  }
}

case class VarDeclaration(val varType:VarType, val name:String) extends Declaration{
  val children:List[Node] = List(name,varType)

  val semanticAction = SemanticAction(
    (attributes: SemanticAttributes) => {
      validateDuplicates(attributes)
      SymbolTable.put((name,attributes),varType) //place this symbol in SymTable
    }
  )
}

case class StructDeclaration(val name:String, val value:Struct) extends Declaration{  
  val children:List[Node] = List(name,value) 

  val getUnderlyingType = () => "Struct"

  //validate inner elements
  val semanticAction = SemanticAction(
    (attributes: SemanticAttributes) => {
      validateDuplicates(attributes)
      SymbolTable.put((name,attributes),value) //place this symbol in SymTable
      value.semanticAction(name) //validate the struct, no duplicates inside
    }
  )
}

case class MethodDeclaration(val methodType:VarType,val name:String,val parameters:List[Parameter],val codeBlock:Block) extends Declaration{
  val children:List[Node] = List[Node](methodType,name)++parameters:+codeBlock  

  val semanticAction = SemanticAction(
    (attributes : SemanticAttributes) => {
      validateDuplicates(attributes)

      //check no duplicates in parameter names
      val ps = parameters.map(_.name)
      if (ps.diff(ps.toSet.toSeq) != Nil) //ugly code, TODO: write a "nub" function instead
	SemanticError("duplicate parameter names found in method "+name)
      
      //val symbolAttributes: SymbolAttributes = (methodType,parameters)      
      SymbolTable.put((this.name,attributes),this)
      println("Metio metodo! "+this.name)
      codeBlock.semanticAction(SemanticAttributes(Some(this.name)))            
    }
  )
}

trait VarType extends Expression with InnerType{
  val children:List[Node] = Nil
}

class PrimitiveType[+T](implicit m:Manifest[T]) extends VarType with NoSemanticAction with InnerType{
  override def toString = "PrimitiveType["+m.toString+"]"
  val getUnderlyingType = () => m.toString
}

object void extends VarType with NoSemanticAction{
  val getUnderlyingType = () => "void"
}

object PrimitiveBoolean extends PrimitiveType[Boolean]
object PrimitiveInt extends PrimitiveType[Int]
object PrimitiveChar extends PrimitiveType[Char]

//basic types
case class struct(val name:String) extends VarType { //the name of the struct 
  val semanticAction = SemanticAction(
    attributes => {
      if (!(SymbolTable.contains((this.name,attributes)) || SymbolTable.contains((this.name,attributes))))
	SemanticError("\""+this.name+"\" struct could not be found in scope global or "+attributes.scope)
    }
  )

  val getUnderlyingType = () => "struct_"+name
}

trait TypeConstructor[T] extends VarType

case class KArray[U <: AnyVal](implicit m:Manifest[U]) extends TypeConstructor[U] with NoSemanticAction{
  override def toString = "KArray["+m.toString+"]"

  val getUnderlyingType = () => m.toString
}

case class Struct(val value:List[VarDeclaration]) extends TypeConstructor[List[VarDeclaration]]{
  override val children = value

  val semanticAction = SemanticAction(
    (attributes: SemanticAttributes) => {
      //perform a semantic action in each one
      value.foreach( _.semanticAction(attributes))
    }
  )

  val getUnderlyingType = () => value.mkString(",")
}

abstract class Parameter extends Node{
  val varType:PrimitiveType[_]
  val name:String
  override val children:List[Node] = List(varType,name)
}

case class PrimitiveTypeParameter(val varType:PrimitiveType[AnyVal], val name:String) extends Parameter

case class PrimitiveArrayParameter(val varType:PrimitiveType[AnyVal], val name:String) extends Parameter

case class Block(val varDeclarations:List[VarDeclaration], val statements:List[Statement]) extends Statement with SemanticRule{
  override val children:List[Node] = varDeclarations++statements

  val semanticAction = SemanticAction(
    (attributes: SemanticAttributes) => {
      varDeclarations.foreach( _.semanticAction(attributes))
      statements.foreach(_ match{
	case illegalLocation:Location => throw SemanticError("Illegal location statement: "+illegalLocation)
	case legalStatement => legalStatement.semanticAction(attributes)
      })
    }
  )
}

abstract class Statement extends Node with SemanticRule{
  val children:List[Node]
}

trait ConditionStatement extends Statement{
  val expression:Expression
  val codeBlock:Block
  
  //assert on the type of the expression, it has to be boolean
  val semanticAction = SemanticAction(
    (attributes : SemanticAttributes) => {
      //expression has to be reducible to true or false
      expression.getUnderlyingType() match{
	case "Boolean" => {} //is good, do nothing
	case innerType => throw SemanticError("expression of type Boolean needed but "+innerType+" was found")
      }	

      //check code block as well
    codeBlock.semanticAction(attributes)
    }
  )
}

case class IfStatement(val expression:Expression,val codeBlock:Block, val elseBlock:Option[Block] = None) extends ConditionStatement{
  val children:List[Node] = elseBlock match{
    case Some(elseBlock) => List(expression,codeBlock,elseBlock)
    case _ => List(expression,codeBlock)
  }  
}

case class WhileStatement(val expression:Expression,val codeBlock:Block) extends ConditionStatement{
  val children:List[Node] = List(expression,codeBlock)
}

case class MethodCall(val name:String,val arguments:List[Expression]) extends Expression{
  val children:List[Node] = List[Node](name)++arguments

  val getUnderlyingType = () => "void"

  val semanticAction = SemanticAction(
    attributes => {
      //the method name must exist and it must be of type MethodDeclaration, and the number of the arguments must be the same and of the same type
      SymbolTable.get((this.name,"Program")) match { //this is HARDCODED! attributes.scope should be List[Scope]
	case Some(attributes) => attributes match{
	  case method:MethodDeclaration => {
	    if (arguments.size!=method.parameters.size)
	      throw SemanticError("the number of arguments do not match the number of the method parameters")
	    
	    arguments.zip(method.parameters).foreach(
	      argumentAndParameter => {
		val argument:Expression = argumentAndParameter._1
		val parameter:Parameter = argumentAndParameter._2
		if (argument.getUnderlyingType != parameter.varType.getUnderlyingType)
		  throw SemanticError("Type Error: argument "+argument+" has type "+argument.getUnderlyingType+", expected: "+parameter.varType.getUnderlyingType)
	      })
	  }
	  case _ => throw SemanticError(name+" is not a method")
	}
	case _ => throw SemanticError("the symbol "+name+" could not be found")
      }
    }
  )
}

case class ReturnStatement(val expression:Option[Expression]) extends Statement{
  val children:List[Node] = expression match{
    case Some(exp) => List(exp)
    case _ => Nil
  }

  val semanticAction = SemanticAction(
    attributes => {
      //the type of the return expression must be the same as declared in method declaration
      SymbolTable.get((attributes,"Program")) match{ //HARDCODED, see this#MethodCall#semanticAction
	case Some(node) => node match {
	  case method:MethodDeclaration => {
	    expression match{
	      case Some(exp) => 
		if (method.methodType.getUnderlyingType != exp.getUnderlyingType)
		  throw SemanticError("the type of "+expression+" must be "+method.methodType.getUnderlyingType()+"; found: "+exp.getUnderlyingType())
	      case _ => 
		if (method.methodType.getUnderlyingType != "void")
		  throw SemanticError("the type of "+expression+" must be "+method.methodType.getUnderlyingType()+"; found: void")
	    }	    	    
	  }
	  case _ => throw SemanticError("could not found the return type of the method-. Found pseudo-method: "+node+" .. ")
	}
	case _ => throw SemanticError("cannot find symbol for this statement: return "+expression+" attributes: "+attributes)
      }
    }
  )
}

case class Assignment(val location:Location,val expression:Expression) extends Statement{
  val children:List[Node] = List(location,expression)

  val semanticAction = SemanticAction(
    attributes => {
      if (location.getUnderlyingType() != expression.getUnderlyingType())
	throw SemanticError("cannot assign expression of type "+expression.getUnderlyingType()+" to "+location.name+" of declared type "+location.getUnderlyingType())
    }
  )
}

abstract class Location extends Expression with InnerType{
  val name:String
  val optionalMember:Option[Location]

  val getUnderlyingType: () => String = () => optionalMember match{
    case Some(member) => member.getUnderlyingType()
    case _ => SymbolTable.getSymbolName(this.name) match{
      case Some(node) => node match{
	case symbolWithInnerType:InnerType => symbolWithInnerType.getUnderlyingType()
	case _ => throw SemanticError("Internal problem: "+name+" must have an inner type")
      }
      case _ => throw SemanticError("cannot find symbol "+this.name)
    }
  }
}

case class SimpleLocation(val name:String, val optionalMember:Option[Location] = None) extends Location {
  val children:List[Node] = optionalMember match{
    case Some(member) => List(name,member)
    case _ => List(s(name))
  }  

  //semantic action must be, assert on the existence of Some(optionalMember
  val semanticAction = SemanticAction(
    attributes => {
      throw SemanticError(this+" pendiente")
    }
  )
}

case class ArrayLocation(val name:String, val index:Expression, val optionalMember:Option[Location] = None) extends Location{
  val children:List[Node] = optionalMember match{
    case Some(member) => List(name,index,member)
    case _ => List(name)
  }

//val getUnderlyingType = "pendiente"

  val semanticAction = SemanticAction(
    attributes => {
      throw SemanticError(this+"pendiente")
    }
  )
}

abstract class Expression extends Statement with InnerType with SemanticRule

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

  val semanticAction = SemanticAction(
    attributes => {
      //both exp1 and exp2 must have the same inner type

      if (exp1.getUnderlyingType != exp2.getUnderlyingType)
	throw SemanticError("cannot operate "+ exp1 +" and "+exp2 +"; both must be of the same type. Found: "+exp1.getUnderlyingType+" and "+exp2.getUnderlyingType)
    }
  )
}

trait UnaryOperation[T] extends ExpressionOperation[T] with NoSemanticAction{
  val exp:Expression
  val children:List[Expression] = List(exp)
}

case class ExpressionAdd(val exp1:Expression,val exp2:Expression)(implicit val m:Manifest[Int]) extends BinaryOperation[Int] with InnerInt

case class ExpressionSub(val exp1:Expression,val exp2:Expression)(implicit val m:Manifest[Int]) extends BinaryOperation[Int] with InnerInt
case class ExpressionMult(val exp1:Expression, val exp2:Expression)(implicit val m:Manifest[Int]) extends BinaryOperation[Int] with InnerInt
case class ExpressionDiv(val exp1:Expression, val exp2:Expression)(implicit val m:Manifest[Int]) extends BinaryOperation[Int] with InnerInt
case class ExpressionMod(val exp1:Expression, val exp2:Expression)(implicit val m:Manifest[Int]) extends BinaryOperation[Int] with InnerInt

case class ExpressionAnd(val exp1:Expression, val exp2:Expression)(implicit val m:Manifest[Boolean]) extends BinaryOperation[Boolean] with InnerBool
case class ExpressionOr(val exp1:Expression, val exp2:Expression)(implicit val m:Manifest[Boolean]) extends BinaryOperation[Boolean] with InnerBool

case class ExpressionLessOrEquals(val exp1:Expression, val exp2:Expression)(implicit val m:Manifest[Boolean]) extends BinaryOperation[Boolean] with InnerBool
case class ExpressionLess(val exp1:Expression, val exp2:Expression)(implicit val m:Manifest[Boolean]) extends BinaryOperation[Boolean] with InnerBool
case class ExpressionGreater(val exp1:Expression, val exp2:Expression)(implicit val m:Manifest[Boolean]) extends BinaryOperation[Boolean] with InnerBool
case class ExpressionGreaterOrEquals(val exp1:Expression, val exp2:Expression)(implicit val m:Manifest[Boolean]) extends BinaryOperation[Boolean] with InnerBool
case class ExpressionEquals(val exp1:Expression, val exp2:Expression)(implicit val m:Manifest[Boolean]) extends BinaryOperation[Boolean] with InnerBool
case class ExpressionNotEquals(val exp1:Expression, val exp2:Expression)(implicit val m:Manifest[Boolean]) extends BinaryOperation[Boolean] with InnerBool

case class NegativeExpression(val exp:Expression)(implicit val m:Manifest[Int]) extends UnaryOperation[Int] with InnerInt
case class NotExpression(val exp:Expression)(implicit val m:Manifest[Boolean]) extends UnaryOperation[Boolean] with InnerBool

abstract class Literal[+T](implicit m:Manifest[T]) extends Expression{
  val literal:T
  override val children = Nil
  override def toString = "Literal["+m+"]"
}

case class IntLiteral(val literal:Int)(implicit val m:Manifest[Int]) extends Literal[Int] with InnerInt with SemanticActionPendiente

case class CharLiteral(val literal:Char)(implicit val m:Manifest[Char]) extends Literal[Char] with InnerChar with SemanticActionPendiente

case class BoolLiteral(val literal:Boolean)(implicit val m:Manifest[Boolean]) extends Literal[Boolean] with InnerBool with SemanticActionPendiente

trait SemanticActionPendiente extends SemanticRule{
  self: Node =>

  val semanticAction = SemanticAction(
    attributes => {
      throw SemanticError(this+"pendiente")
    }
  )
}
