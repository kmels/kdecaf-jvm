package kmels.uvg.kdecaf.compiler

import scala.collection.mutable.HashMap
import parsing.ast._
import types.aliases._

/*
 * The Symbols table
 *
 * @author Carlos Lopez
*/
object SymbolTable extends SymbolTable

trait SymbolTable extends HashMap[Symbol,Node]{
  def containsName(name:String):Boolean = this.keySet.exists(_._1==name)

  def getSymbolName(name:String):Option[Node] = this.find(_._1._1 == name) match{
    case Some(symbolToSymbolAttributes) => Some(symbolToSymbolAttributes._2)
    case _ => None    
  }
}
