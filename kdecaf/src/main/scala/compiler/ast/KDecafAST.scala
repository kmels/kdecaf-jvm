package kmels.uvg.kdecaf.compiler.parsing.ast

import kmels.uvg.kdecaf.compiler._
import codegen._
import semantics._
import kmels.uvg.kdecaf.compiler.types.{aliases => typeAliases}
import typeAliases._
import scala.util.parsing.input.{Positional,Position}

/**
 * ASTs class nodes
 *
 * @author Carlos Lopez
 * @version 2.0
 * @since 1.0
 */
package object IntermediateCodeMappers{
  val globalFieldConstructor : Int => codegen.GlobalField = index => GlobalField(index)
  val localFieldConstructor : Int => codegen.LocalField = index => LocalField(index)
  
  implicit def varTypeToJVMType(v:VarType):codegen.JVMTypes.JVMType = v.getUnderlyingType() match{
    case "Int" | "Char" | "Boolean" => codegen.JVMTypes.INT
    case "void" => codegen.JVMTypes.VOID
  }

  implicit def scopeToScopeName(scope:KScope):String = scope.scopeName
  implicit def statementToStatementList(statement:codegen.Statement):List[codegen.Statement] = List(statement)
  implicit def statementToBody(statement:codegen.Statement):codegen.Body = codegen.MethodBody(List(statement))
  implicit def statementListToBody(statements:List[codegen.Statement]):codegen.Body = codegen.MethodBody(statements)
  implicit def bodyToStatements(body:codegen.Body):List[codegen.Statement] = body.computations
}

import IntermediateCodeMappers._

trait NoCodeMapping{
  def imap(scope:KScope):codegen.Body = codegen.MethodBody(Nil)
}

trait KScope extends InnerType{
  val scopeName:String

  def getField(varName:String):Field = CodegenFields.getField(varName,scopeName)
}

trait Node extends Positional with InnerType{
  override def toString = getClass.getName
  implicit def s(s:String):Node = new StringWrapper(s)

  val children: List[Node]   

  //string wrapper for listing nodes
  class StringWrapper(val s:String) extends Node with NoInnerType{
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

trait InnerString extends InnerType{
  val getUnderlyingType = () => "String"
}

trait InnerTypeVoid extends InnerType{
  val getUnderlyingType = () => "void"
}

trait NoInnerType extends InnerType{
  val getUnderlyingType = () => "Nothing"
}

case class Program(val name:String, val declarations:List[Declaration]) extends Node with SemanticRule with NoInnerType{ 
  val children = declarations

  val semanticAction = SemanticAction(
    attributes => {
      val scope = this.name //attributes with the name of the scope
      val declarationResults:List[SemanticResult] = declarations.map(_.semanticAction(scope)) //get declaration semantic results
      
      //there has to be a method named "main" of type "void" within the declarations
      val existsMainMethod:Boolean = declarations.exists(declaration => declaration match{
	case methodDeclaration:MethodDeclaration => methodDeclaration.name == "main" && methodDeclaration.methodType.getUnderlyingType() == "void"
	case _ => false
      })

      if (!existsMainMethod)
	SemanticResults((declarationResults :+ SemanticError(this.name+" must have a method named \"main\" of type void")) : _*)
      else
	SemanticResults(declarationResults : _* )
    }
  )

  /**
   * Maps to intermediate code
   */
  def imap:List[codegen.ClassMember] = {
    var globalVarsCounter = 0

    declarations.map(_ match{
      case varDcl:VarDeclaration => {
	globalVarsCounter += 1
	varDcl.imap[GlobalField](globalFieldConstructor,globalVarsCounter)	
      }
      case methodDecl:MethodDeclaration => {
	methodDecl.imap
      }
      case x => {
	println("HEY: "+x)
	null
      }
    })
  }

  /**
   * Maps to jasmin code
   */ 
  def jmap(xs:List[codegen.ClassMember]):String = {
    implicit val tabSpaces = 0
    ".class "+this.name + "\n.super java/lang/Object\n"+xs.map(_.jasminCode).mkString("\n")
  }
}

abstract class Declaration extends Node with SemanticRule{
  val name:String
  
  // don't allow duplicate names of variables
  val validateDuplicates: SemanticAction  = SemanticAction(
    declarationScope => {
      //val scope = attributesToScope(attributes)
      if (SymbolTable.contains((name,declarationScope))){
	SemanticError(name+"\" already declared in scope \""+declarationScope+"\"")
      }
	
      else
	SemanticSuccess
    }
  )
}

case class VarDeclaration(val varType:VarType, val name:String) extends Declaration with InnerTypeVoid {
  val children:List[Node] = List(name,varType)

  val semanticAction = SemanticAction(
    varScope => {
      val checkDuplicatesResult = validateDuplicates(varScope)
      SymbolTable.put((name,varScope),varType) //place this symbol in SymTable      
      CodegenFields.place(name,varScope)
		 
      SemanticResults(
	checkDuplicatesResult,varType.semanticAction(varScope)
      )

      checkDuplicatesResult
    }
  )

  def imap[T <: codegen.Field](f: Int => codegen.Field, index:Int):codegen.TwoAddressAssignment = varType.getUnderlyingType() match{
    case "Int" | "Char" | "Boolean"  =>  TwoAddressAssignment(f(index),codegen.Constants.NULL)
  }
}

case class StructDeclaration(val name:String, val value:Struct) extends Declaration{  
  val children:List[Node] = List(name,value) 

  val getUnderlyingType = () => "Struct"

  //validate inner elements
  val semanticAction = SemanticAction(
    structScope => {
      validateDuplicates(structScope)
      SymbolTable.put((name,structScope),value) //place this symbol in SymTable
      value.semanticAction(this.name) //validate the struct, no duplicates inside
    }
  )
}

case class MethodDeclaration(val methodType:VarType,val name:String,val parameters:List[Parameter],val codeBlock:Block) extends Declaration with NoInnerType with KScope { 
  val children:List[Node] = List[Node](methodType,name)++parameters:+codeBlock  

  val scopeName = this.name

  val semanticAction = SemanticAction(
    methodScope => {
      parameters.foreach(p => {
	SymbolTable.put((p.name,this.name),p)
      })
      validateDuplicates(methodScope)
      
      //check no duplicates in parameter names
      val ps = parameters.map(_.name)
      if (ps.diff(ps.toSet.toSeq) != Nil) //ugly code, TODO: write a "nub" function instead
	SemanticError("duplicate parameter names found in method "+name)
      
      parameters.foreach( p => CodegenFields.place(p.name,this.name) )
      //val symbolAttributes: SymbolAttributes = (methodType,parameters)      
      SymbolTable.put((this.name,methodScope),this)
      codeBlock.semanticAction(this.name)
    }
  )

  def imap:Method = codegen.Method(
    methodType,
    this.name,{
      var parameterCounter = 0
      this.parameters.map(p => { 
	parameterCounter += 1
	codegen.Parameter(p.varType, parameterCounter)})      
    },{
      codeBlock.imap(this)
    }    
  )
}

trait VarType extends Expression with InnerType with NoCodeMapping{
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
    scope => {
      if (!(SymbolTable.contains((this.name,scope)) || SymbolTable.contains((this.name,scope))))
	SemanticError("\""+this.name+"\" struct could not be found in scope global or "+scope)
      else
	SemanticSuccess
    }
  )

  val getUnderlyingType = () => "struct_"+name
}

trait TypeConstructor[+T] extends VarType

case class KArray[U <: AnyVal](val size:Int)(implicit m:Manifest[U]) extends TypeConstructor[U] with SemanticRule{
  override def toString = "KArray["+m.toString+"]"

  val semanticAction = SemanticAction(
    scope => {
      if (!(size > 0))
	SemanticError("Array size should be greater than 0, found: "+size)
      else
	SemanticSuccess
    }
  )
  val getUnderlyingType = () => m.toString
}

case class Struct(val value:List[VarDeclaration]) extends TypeConstructor[List[VarDeclaration]]{
  override val children = value

  val semanticAction = SemanticAction(
    scope => {
      //perform a semantic action in each one
      SemanticResults(
	value.map( _.semanticAction(scope)) : _*
      )
    }
  )

  val getUnderlyingType = () => value.mkString(",")
}

abstract class Parameter extends Node with InnerType{
  val varType:PrimitiveType[_]
  val name:String
  override val children:List[Node] = List(varType,name)
  val getUnderlyingType = varType.getUnderlyingType
}

case class PrimitiveTypeParameter(val varType:PrimitiveType[AnyVal], val name:String) extends Parameter 

case class PrimitiveArrayParameter(val varType:PrimitiveType[AnyVal], val name:String) extends Parameter

case class Block(val varDeclarations:List[VarDeclaration], val statements:List[Statement]) extends Statement with SemanticRule with InnerTypeVoid{
  override val children:List[Node] = varDeclarations++statements

  val semanticAction = SemanticAction(
    blockScope => {
      val varDeclarationsSemanticResults = varDeclarations.map(_.semanticAction(blockScope))
      //varDeclarations.foreach( _.semanticAction(blockScope))

      val statementResults = statements.map(_ match{
	  case illegalLocation:Location => SemanticError("Illegal location statement: "+illegalLocation)
	  case legalStatement => legalStatement.semanticAction(blockScope)
	}) 

      SemanticResults(
	(varDeclarationsSemanticResults ++ statementResults) : _*
      )
    }
  )    

  def imap(scope:KScope):codegen.MethodBody = {
    var localVariablesCounter = 0
    val computations:List[codegen.Statement] = statements.flatMap(_ match {
      case returnS:ReturnStatement => returnS.imap(scope)
      case printStatement:PrintStatement[_] => printStatement.imap(scope)
      case assignment:Assignment => assignment.imap(scope).computations
      case EmptyExpression => None
      case conditionStatement:ConditionStatement => conditionStatement.imap(scope)
      case e => {
      println("AGUAS, encontro: "+e)
	//codegen.DummyBody(e.toString).assignments
	None
      }
    })
    codegen.MethodBody(computations)
  }
}

abstract class Statement extends Node with SemanticRule {
  val children:List[Node]

  def imap(scope:KScope): codegen.Body
}

trait ConditionStatement extends Statement{
  val expression:Expression
  val codeBlock:Block

  def nextBlockOfCode(scope:KScope):Option[codegen.Body]

  def imap(scope:KScope):codegen.Body  = {
    val blockStatements = codeBlock.imap(scope)       
    val predicate:codegen.Body = expression.imap(scope)
    val nextBlockComputations:List[codegen.Statement] = nextBlockOfCode(scope) match { case Some(body) => body.computations ; case _ => Nil }
    val nextBlockLabel = CodegenLabelMaker(scope)
    val predicateAndGotoStatements:List[codegen.Statement] = predicate.computations :+
    codegen.GotoStatement(
      predicate.lastEvaluatedField,      
      nextBlockLabel
    ) 

    //println("NEXT BLOCK: "+nextBlockLabel+": \n\n\t\t"+nextBlockComputations.mkString("\n\t\t"))
    predicateAndGotoStatements ++ blockStatements.computations ++ List(nextBlockLabel) ++ nextBlockComputations
  }
    
  //assert on the type of the expression, it has to be boolean
  val assertOnBooleanExprAndDoBlock = SemanticAction(
    scope => {
      //expression has to be reducible to true or false
      expression.getUnderlyingType() match{
	case "Boolean" => {} //is good, do nothing
	case innerType => SemanticError("expression of type Boolean needed but "+innerType+" was found")
      }	
      
	//check code block as well
      codeBlock.semanticAction(scope)
    }
  )
}

trait PrintStatement[T] extends Statement with NoSemanticAction{
  val exp:T
  override def imap(scope:KScope):codegen.Body
  val getUnderlyingType  = () => "void"
  val children = Nil  
}

case class PrintString(val exp:String) extends PrintStatement[String]{
  println("PRINT STRING: "+exp)
  override def imap(scope:KScope):codegen.Body = codegen.PrintLineStatement(exp)
}

case class PrintExpression(val exp:Expression) extends PrintStatement[Expression]{
  println("PRINT EEXP: "+exp)
  override def imap(scope:KScope):codegen.Body = codegen.PrintIntegerStatement(exp.imap(scope).lastEvaluatedField)
}

case class IfStatement(val expression:Expression,val codeBlock:Block, val elseBlock:Option[Block] = None) extends ConditionStatement with InnerTypeVoid{
  val children:List[Node] = elseBlock match{
    case Some(elseBlock) => List(expression,codeBlock,elseBlock)
    case _ => List(expression,codeBlock)
  } 

  def nextBlockOfCode(scope:KScope):Option[codegen.Body] = elseBlock match { case Some(block) => Some(block.imap(scope)) ; case _ => None}

  val semanticAction = SemanticAction(
    scope => {
      val assert1Result = assertOnBooleanExprAndDoBlock(scope)
      val elseResult = elseBlock match{
	case Some(elseBlock) => elseBlock.semanticAction(scope)
	case _ => SemanticSuccess
      }
      SemanticResults(assert1Result,elseResult)
    }
  )
}

case class WhileStatement(val expression:Expression,val codeBlock:Block) extends ConditionStatement with InnerTypeVoid{
  val children:List[Node] = List(expression,codeBlock)

  val semanticAction = assertOnBooleanExprAndDoBlock

  def nextBlockOfCode(scope:KScope) = None
}

case class MethodCall(val name:String,val arguments:List[Expression]) extends Expression with NoCodeMapping{
  val children:List[Node] = List[Node](name)++arguments

  val getUnderlyingType = () => SymbolTable.get(this.name,"Program") match{
    case Some(node) => node match{
      case methodDcl:MethodDeclaration => methodDcl.methodType.getUnderlyingType()
      case _ => "Nothing"
    }
    case _ => "Nothing"
  }

  override val semanticAction = SemanticAction(
    attributes => {
      //the method name must exist and it must be of type MethodDeclaration, and the number of the arguments must be the same and of the same type
      SymbolTable.get((this.name,"Program")) match { //this is HARDCODED! attributes.scope should be List[Scope]
	case Some(attributes) => attributes match{
	  case method:MethodDeclaration => {
	    val argsSizeResult = 
	      if (arguments.size!=method.parameters.size)
		SemanticError("the number of arguments do not match the number of the method parameters")
	      else
		SemanticSuccess

	    val argumentResults:List[SemanticResult] = arguments.zip(method.parameters).map(
		argumentAndParameter => {
		  val argument:Expression = argumentAndParameter._1
		  val parameter:Parameter = argumentAndParameter._2
		  typesShouldBeEqualIn(
		    argument,parameter.varType,
		    "Type Error: argument "+argument+" has type "+argument.getUnderlyingType()+", expected: "+parameter.varType.getUnderlyingType()
		  )
		})

	    SemanticResults(
	      (argsSizeResult :: argumentResults) : _*
	    )
	  }
	  case _ => SemanticError(name+" is not a method")
	}
	case _ => SemanticError("the symbol "+name+" could not be found")
      }
    }
  )
}

case class ReturnStatement(val expression:Option[Expression]) extends Statement with InnerTypeVoid{
  val children:List[Node] = expression match{
    case Some(exp) => List(exp)
    case _ => Nil
  }

  def imap(scope:KScope):codegen.Body = {
    val returnJvmType:codegen.JVMTypes.JVMType = SymbolTable((scope.scopeName,"Program")).asInstanceOf[MethodDeclaration].methodType

    expression match {
      case Some(exp) => {
	val expressionBody = exp.imap(scope)
	//println("RETURN EXP BODY: "+expression)
	  expressionBody.computations :+ codegen.Return(
	    returnJvmType,Some(expressionBody.lastEvaluatedField)
	  )
      }
      case _ => {
	List(codegen.Return(returnJvmType,None))
      }
    }        
  }

  val semanticAction = SemanticAction(
    attributes => {
      //the type of the return expression must be the same as declared in method declaration
      SymbolTable.get((attributes,"Program")) match{ //HARDCODED, see this#MethodCall#semanticAction
	case Some(node) => node match {
	  case method:MethodDeclaration => {
	    expression match{
	      case Some(exp) => {
		val returnInnerType:InnerType = exp match{
		  case location:Location => SymbolTable.getSymbolName(location.name).getOrElse(EmptyExpression)
		  case _ => exp
		}
		
		typesShouldBeEqualIn(
		  method.methodType,exp,		    
		  "found: "+exp.getUnderlyingType()+"; expected: "+method.methodType.getUnderlyingType()		  
		)
	      }	      
	      case _ => 
		if (method.methodType.getUnderlyingType() != "void")
		  SemanticError("found: void; expected: "+method.methodType.getUnderlyingType())
		else
		  SemanticSuccess
	    }	    	    
	  }
	  case _ => SemanticError("could not found the return type of the method-. Found pseudo-method: "+node+" .. ")
	}
	case _ => SemanticError("cannot find symbol for this statement: return "+expression+" attributes: "+attributes)
      }
    }
  )
}

case class Assignment(val location:Location,val expression:Expression) extends Statement with InnerTypeVoid{
  val children:List[Node] = List(location,expression)

  val semanticAction = SemanticAction(
    attributes => {
      val typeEqualityResult = {	
	val x = typesShouldBeEqualIn(
	  location,expression,
	  "cannot assign expression of type "+expression.getUnderlyingType()+" to "+location.name+" of declared type "+location.getUnderlyingType()
	)

	x match {
	  case SemanticError(x) => println("semantic error: | location="+location+", location.name="+location.name)
	  case _ => {}
	}
	x
      }

      SemanticResults(typeEqualityResult,location.semanticAction(attributes),expression.semanticAction(attributes))
    }
  )

  def imap(scope:KScope):codegen.MethodBody = {
    val expressionBody = expression.imap(scope)

    val thisAssignment = TwoAddressAssignment(
	CodegenFields.getField(location.name,scope.scopeName),
	expressionBody.lastEvaluatedField
    )
        
    codegen.MethodBody( expressionBody.computations :+ thisAssignment )
  }
}

abstract class Location extends Expression with InnerType{
  val name:String
  val optionalMember:Option[Location]  
}

case class SimpleLocation(val name:String, val optionalMember:Option[Location] = None) extends Location with SemanticRule {
  val children:List[Node] = optionalMember match{
    case Some(member) => List(name,member)
    case _ => List(s(name))
  }  
  
  def imap(scope:KScope) = CodegenFields.getField(this.name,scope.scopeName)

  //in case the optional member is trying to get fetched and it doesn't exist 
  object SemanticErrorType extends Enumeration{
    type SemanticErrorType = Value
    val NotAStruct,LocationNotFound,MemberDoesNotExist = Value
  }
  import SemanticErrorType._

  /**
   * returns (None,_) if member does not exist or this location is not an struct 
   */
  private def getInnerMemberType:(Option[String],Option[SemanticErrorType]) = {
    val structName:(Option[String],Option[SemanticErrorType]) = SymbolTable.getSymbolName(this.name) match{
      case Some(node) => node match{
	case struct:struct => (Some(struct.name),None)
	  case _ => (None,Some(SemanticErrorType.NotAStruct)) //this is not a struct
      }
      case _ => (None,Some(SemanticErrorType.LocationNotFound))  //this variable does not exist
    }
    
    structName._1 match{
      case Some(structName) => SymbolTable.get(optionalMember.get.name,structName) match{
	case Some(node) => (Some(node.getUnderlyingType()),None)
	  case _ => (None,Some(SemanticErrorType.MemberDoesNotExist))
      }
      case _ => structName
    } 
  }

  val getUnderlyingType: () => String = () => optionalMember match{
    case Some(member) => getInnerMemberType._1 match{ case None => "Nothing" ; case underlyingMemberType => underlyingMemberType.get}
    case _ => SymbolTable.getSymbolName(this.name) match{
      case Some(node) => node.getUnderlyingType()
      case _ => "Nothing"
    }
  }

  //semantic action must be, assert on the existence of Some(optionalMember
  val semanticAction = SemanticAction(
    currentScope => {      
      getUnderlyingType() match{
	case "Nothing" => optionalMember match{
	  case Some(member) => getInnerMemberType._2.get match{
	    case NotAStruct => SemanticError(this.name+" is not a struct")
	    case LocationNotFound => SemanticError(this.name+" can not be found")
	    case MemberDoesNotExist => SemanticError(member.name+" is not a member of "+this.name)
	  }
	  case _ => SemanticSuccess
	}
	case _ => SemanticSuccess
      } //end underlying type match
    } //end semantic action
  )
}

case class ArrayLocation(val name:String, val index:Expression, val optionalMember:Option[Location] = None) extends Location with NoCodeMapping{
  val children:List[Node] = optionalMember match{
    case Some(member) => List(name,index,member)
    case _ => List(name)
  }

  //in case the optional member is trying to get fetched and it doesn't exist 
  object SemanticErrorType extends Enumeration{
    type SemanticErrorType = Value
    val LocationIsNotArray,LocationNotFound,NotAStruct = Value
  }
  import SemanticErrorType._

  private def arrayDeclaration:(Option[KArray[_]],Option[SemanticErrorType]) = SymbolTable.getSymbolName(this.name) match{
    case Some(node) => node match{
      case array:KArray[_] => (Some(array),None)
      case _ => (None,Some(SemanticErrorType.LocationIsNotArray))
    }
    case _ => (None,Some(SemanticErrorType.LocationNotFound)) //this variable does not exist
  }
  
  private def getInnerMemberType:(Option[String],Option[SemanticErrorType]) = optionalMember match{
    case Some(innerMember) => arrayDeclaration._1 match{
      case Some(arrayDeclaration) => arrayDeclaration.getUnderlyingType() match{
	case "Struct" => (Some(arrayDeclaration.getUnderlyingType()),None)
	case _ => (Some("eeeeeeee..."),Some(NotAStruct))
      }
      case _ => (None,arrayDeclaration._2)
    }
    case _ => (None,None) //this shouldn't happen as we'll never try to get inner member type if it's None
  }
  
  val getUnderlyingType: () => String = () => optionalMember match{
    case Some(member) => getInnerMemberType._1 match{
      case Some(innerType) => innerType
      case _ => "Nothing"
    }
    case _ => arrayDeclaration._1 match{
      case Some(varDeclaration) => varDeclaration.getUnderlyingType()
      case _ => "Nothing"
    }
  }
  
  val semanticAction = SemanticAction(
    attributes => {
      val isArrayResult = arrayDeclaration._1 match{
	case Some(varDecl) => SemanticSuccess
	case _ => arrayDeclaration._2.get match{
	  case SemanticErrorType.LocationIsNotArray => SemanticError(this.name+" is not an array")
	  case SemanticErrorType.LocationNotFound => SemanticError(this.name+" could not be found")
	}
      }

      val indexIsInteger = index.getUnderlyingType() match{
	case "Int" => SemanticSuccess
	case _ => SemanticError("found: "+index.getUnderlyingType()+"; expected: Int")
      }

      val optionalMemberResult:SemanticResult = optionalMember match{
	case Some(member) => getInnerMemberType._1 match{
	  case Some(structName) => {
	    //pendiente
	    SemanticSuccess
	  }
	  case _ => getInnerMemberType._2.get match{
	    case SemanticErrorType.LocationIsNotArray => SemanticSuccess //is is already covered by isArrayResult
	    case SemanticErrorType.LocationNotFound => SemanticSuccess   //is is already covered by isArrayResult
	    case SemanticErrorType.NotAStruct => SemanticError(this.name+" is not a struct")
	  }
	}
	case _ => SemanticSuccess
      }

      SemanticResults(isArrayResult,indexIsInteger,optionalMemberResult)
    }
  )
}

abstract class Expression extends Statement with InnerType with SemanticRule{
  def imap(scope:KScope):codegen.Body
}

object EmptyExpression extends Expression with NoInnerType with NoSemanticAction with NoCodeMapping{
  val children = Nil
}

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

trait BinaryOperation[T] extends ExpressionOperation[T] {
  val exp1:Expression
  val exp2:Expression

  val children:List[Expression] = List(exp1,exp2)

  val semanticAction = SemanticAction(
    attributes => {
      //both exp1 and exp2 must have the same inner type            
      typesShouldBeEqualIn(
	exp1,exp2,
	"cannot operate "+ exp1 +" and "+exp2 +"; both must be of the same type. Found: "+exp1.getUnderlyingType+" and "+exp2.getUnderlyingType
      )
    }
  )

  val codegenOperator:codegen.Operators.Operator

  def imap(scope:KScope):codegen.MethodBody = {
    val expression2Body:codegen.Body = exp2.imap(scope)
    val expression1Body:codegen.Body = exp1.imap(scope)

    val thisComputation = ThreeAddressAssignment(
      CodegenFields.placeAndGetTempField(scope.scopeName),
	expression1Body.lastEvaluatedField,
	expression2Body.lastEvaluatedField,
	codegenOperator
    )

    codegen.MethodBody(expression1Body.computations ++ expression2Body.computations :+ thisComputation)
  }
}

trait UnaryOperation[T] extends ExpressionOperation[T] with NoSemanticAction with NoCodeMapping{
  val exp:Expression
  val children:List[Expression] = List(exp)

}

case class ExpressionAdd(val exp1:Expression,val exp2:Expression)(implicit val m:Manifest[Int]) extends BinaryOperation[Int] with InnerInt{val codegenOperator = codegen.Operators.ADDITION }

case class ExpressionSub(val exp1:Expression,val exp2:Expression)(implicit val m:Manifest[Int]) extends BinaryOperation[Int] with InnerInt{val codegenOperator = codegen.Operators.SUBSTRACTION}
case class ExpressionMult(val exp1:Expression, val exp2:Expression)(implicit val m:Manifest[Int]) extends BinaryOperation[Int] with InnerInt{val codegenOperator = codegen.Operators.MULTIPLICATION}
case class ExpressionDiv(val exp1:Expression, val exp2:Expression)(implicit val m:Manifest[Int]) extends BinaryOperation[Int] with InnerInt{val codegenOperator = codegen.Operators.DIVISION}
case class ExpressionMod(val exp1:Expression, val exp2:Expression)(implicit val m:Manifest[Int]) extends BinaryOperation[Int] with InnerInt{val codegenOperator = codegen.Operators.MOD}

case class ExpressionAnd(val exp1:Expression, val exp2:Expression)(implicit val m:Manifest[Boolean]) extends BinaryOperation[Boolean] with InnerBool{val codegenOperator = codegen.Operators.AND}
case class ExpressionOr(val exp1:Expression, val exp2:Expression)(implicit val m:Manifest[Boolean]) extends BinaryOperation[Boolean] with InnerBool{val codegenOperator = codegen.Operators.OR}

case class ExpressionLessOrEquals(val exp1:Expression, val exp2:Expression)(implicit val m:Manifest[Boolean]) extends BinaryOperation[Boolean] with InnerBool{val codegenOperator = codegen.Operators.LESSEQUALSTHAN}
case class ExpressionLess(val exp1:Expression, val exp2:Expression)(implicit val m:Manifest[Boolean]) extends BinaryOperation[Boolean] with InnerBool{val codegenOperator = codegen.Operators.LESSTHAN}
case class ExpressionGreater(val exp1:Expression, val exp2:Expression)(implicit val m:Manifest[Boolean]) extends BinaryOperation[Boolean] with InnerBool{val codegenOperator = codegen.Operators.GREATERTHAN}
case class ExpressionGreaterOrEquals(val exp1:Expression, val exp2:Expression)(implicit val m:Manifest[Boolean]) extends BinaryOperation[Boolean] with InnerBool{val codegenOperator = codegen.Operators.GREATEREQUALSTHAN}
case class ExpressionEquals(val exp1:Expression, val exp2:Expression)(implicit val m:Manifest[Boolean]) extends BinaryOperation[Boolean] with InnerBool{val codegenOperator = codegen.Operators.EQUALS}
case class ExpressionNotEquals(val exp1:Expression, val exp2:Expression)(implicit val m:Manifest[Boolean]) extends BinaryOperation[Boolean] with InnerBool{val codegenOperator = codegen.Operators.NEQUALS}

case class NegativeExpression(val exp:Expression)(implicit val m:Manifest[Int]) extends UnaryOperation[Int] with InnerInt
case class NotExpression(val exp:Expression)(implicit val m:Manifest[Boolean]) extends UnaryOperation[Boolean] with InnerBool

abstract class Literal[+T](implicit m:Manifest[T]) extends Expression{
  val literal:T
  override val children = Nil
  override def toString = "Literal["+m+"]"
}

case class IntLiteral(val literal:Int)(implicit val m:Manifest[Int]) extends Literal[Int] with InnerInt with NoSemanticAction {
  def imap(scope:KScope) = {
    codegen.TwoAddressAssignment(
      CodegenFields.placeAndGetTempField(scope),
      codegen.Constants.ConstantField[Int](literal,0)
    ) 
  }
}

case class CharLiteral(val literal:Char)(implicit val m:Manifest[Char]) extends Literal[Char] with InnerChar with NoSemanticAction {
  def imap(scope:KScope) = {
    codegen.TwoAddressAssignment(
      CodegenFields.placeAndGetTempField(scope),
      codegen.Constants.ConstantField[Int](literal.toInt,0)
    )
  }
}

case class StringLiteral(val literal:String)(implicit val m:Manifest[String]) extends Literal[String] with InnerString with NoSemanticAction{
  def imap(scope:KScope) = {
    codegen.TwoAddressAssignment(
      CodegenFields.placeAndGetTempField(scope),
      codegen.StringValue(literal)
    )
  }
}

case class BoolLiteral(val literal:Boolean)(implicit val m:Manifest[Boolean]) extends Literal[Boolean] with InnerBool with NoSemanticAction {
  def imap(scope:KScope) = {
    codegen.TwoAddressAssignment(
      CodegenFields.placeAndGetTempField(scope),
      codegen.Constants.ConstantField[Int](if (!literal) 0 else 1,0)
    )
  }
}
