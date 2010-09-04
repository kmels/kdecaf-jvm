package compiler

import scala.collection.mutable.HashMap
import compiler.parsing.ast._
import compiler.types.{aliases => typeAliases,AttributeList}
import typeAliases._

/*
 * The Symbols table
 *
 * @author Carlos Lopez
*/
//object SymbolTable extends HashMap[(String,String),KDecafAST { def getUnderlyingType:String}]{
object SymbolTable extends HashMap[Symbol,SymbolAttributes]{     
  def place(symbol:Symbol, attributes:SymbolAttributes):Option[SymbolAttributes] = { 
    put(symbol,attributes)
  }

  def containsName(name:String):Boolean = this.keySet.exists(_._1==name)

  def getSymbolName(name:String):Option[SymbolAttributes] = this.find(_._1._1 == name) match{
    case Some(symbolToSymbolAttributes) => Some(symbolToSymbolAttributes._2)
    case _ => None    
  }
}

abstract class SymbolAttributes

case class SymbolAttribute(val node:Attribute) extends SymbolAttributes

case class SymbolAttributes2(val node:(Attribute,Attribute)) extends SymbolAttributes

