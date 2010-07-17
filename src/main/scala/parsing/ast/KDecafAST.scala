package parsing.ast

sealed class KDecafAST

case class Program(val name:String, val declarations:List[Declaration]) extends KDecafAST

abstract class Declaration

case class VarDeclaration(val T:VarType, val id:String) extends Declaration
case class StructDeclaration(val name:String, val struct:struct) extends Declaration

abstract class VarType
//types
case class int() extends VarType
case class char() extends VarType
case class boolean() extends VarType
case class void() extends VarType
case class ArrayOf(val T:VarType, val size:Int) extends VarType
case class struct(val varDeclarations:List[VarDeclaration]) extends VarType
