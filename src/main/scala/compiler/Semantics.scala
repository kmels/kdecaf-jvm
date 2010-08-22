package compiler.semantics

import compiler.{SymbolAttribute,SymbolAttributes,SymbolAttributes2}
import compiler.parsing.ast.KDecafAST
import compiler.types.{aliases => typeAliases,AttributeList}
import typeAliases._

/**
 * Semantic rules
 *
 * @author Carlos Lopez
 * @version 1.0
 * @since 2.0
 */
trait SemanticRule{
  implicit def nodeToAttribute(x:Attribute):SymbolAttributes = SymbolAttribute(x)
  implicit def nodeTupleToAttribute(x:(Attribute,Attribute)):SymbolAttributes = SymbolAttributes2(x)
  implicit def attributeListToAttribute(xs:List[Attribute]):Attribute = AttributeList(xs)
  
  def SemanticAction(f: SemanticAttributes => Unit) = new SemanticAction{
    def apply(attributes: SemanticAttributes) = f(attributes)
  }

  def SemanticError(message: String) = new SemanticError(message)

  val semanticAction:SemanticAction
}

trait SemanticResult

case class SemanticAttributes(val scope:Option[String])

abstract class SemanticAction extends (SemanticAttributes => Unit) with SemanticResult

case class SemanticError(val message:String) extends Exception with SemanticResult{
  override def toString = message
}
