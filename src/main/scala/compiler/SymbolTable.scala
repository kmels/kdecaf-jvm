package compiler

import scala.collection.mutable.HashMap
import compiler.parsing.ast._

/*
 * The Symbols table
 *
 * @author Carlos Lopez
*/
object SymbolTable extends HashMap[(String,String),SymbolAttributes[_]]{ // (name,scopeName) => SymbolAttributes
  
  def place(key:(String,String), varType:VarType):Option[SymbolAttributes[_]] = { 
    println("Metio "+key._1) 
    put(key,SymbolAttributes(varType))
  }
}

case class SymbolAttributes[T](val node:KDecafAST{ def getUnderlyingType:String})(implicit m:Manifest[T]) {
  def getUnderlyingType = m.toString
}
