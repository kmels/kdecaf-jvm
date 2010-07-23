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

abstract class Declaration

case class VarDeclaration(val varType:VarType[_], val id:String) extends Declaration

case class StructDeclaration(val name:String, val value:Struct) extends Declaration

case class MethodDeclaration(val methodType:VarType[_],val name:String,val parameters:List[Parameter],val codeBlock:Block) extends Declaration

abstract class VarType[+T]{
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

case class Parameter(val varType:PrimitiveType[_], val name:String)

case class ArrayParameter(val varType:KArray[_ <: VarType[_]], val name:String)

case class Block(val varDeclarations:List[VarDeclaration], val statements:List[Statement])

abstract class Statement
