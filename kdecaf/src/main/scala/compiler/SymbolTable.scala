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

trait CodegenFields extends HashMap[Symbol,Int]{
  def place(name:String,scope:String):Unit = {    
    val index:Int = getLatestIndexFromScope(scope)
    put((name,scope),index)
  }

  private def getLatestIndexFromScope(scope:String):Int = this.filter(_._1._2 == scope).values.toList.sortBy(i => i) match {
    case head::Nil => head + 1
    case head::tail => tail.last + 1
    case Nil => 1
  }

  def getField(varName:String,scope:String):codegen.Field = get((varName,scope)) match{
    case Some(localFieldIndex) => codegen.LocalField(localFieldIndex)
    case _ => codegen.GlobalField(apply((varName,"Program")))
  }

  def placeAndGetTempField(scope:String):codegen.Field = {    
    val index:Int = getLatestIndexFromScope(scope)
    
    val name = "temp_"+index
    put((name,scope),index)
    codegen.LocalField(index)
  }
}

object CodegenFields extends CodegenFields

object CodegenLabelMaker {
  import IntermediateCodeMappers._
  var counter = 0

  def apply(scope:KScope): codegen.Label  = codegen.Label(scope.scopeName+"_"+counter)
}
