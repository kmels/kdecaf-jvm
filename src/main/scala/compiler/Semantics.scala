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
  self: KDecafAST =>

  import typeAliases.Scope

  implicit def nodeToAttribute(x:Attribute):SymbolAttributes = SymbolAttribute(x)
  implicit def nodeTupleToAttribute(x:(Attribute,Attribute)):SymbolAttributes = SymbolAttributes2(x)
  implicit def nodeTupleWithListToAttribute[T](x:(Attribute,T))(implicit conv: T => Attribute) = SymbolAttributes2((conv(x._2),conv(x._2)))

  implicit def attributeListToAttribute(xs:List[Attribute]):Attribute = AttributeList(xs)

  implicit def attributesToScope(a:SemanticAttributes):Scope = a.scope match{
    case Some(scope) => scope
    case _ => throw new InternalError("scope expected.")
  }
  implicit def scopeToAttributes(s:String):SemanticAttributes = SemanticAttributes(Some(s))
  
  def SemanticAction(f: SemanticAttributes => Unit) = new SemanticAction{
    def apply(attributes: SemanticAttributes) = f(attributes)
  }

  def SemanticError(message: String) = new SemanticError(message,this)

  val semanticAction:SemanticAction
}

trait NoSemanticAction extends SemanticRule{
  self: KDecafAST => 

  val semanticAction = SemanticAction(
    attributes => {}
  )
}
trait SemanticResult

case class SemanticAttributes(val scope:Option[String])

abstract class SemanticAction extends (SemanticAttributes => Unit) with SemanticResult

case class SemanticError(val message:String,val node:KDecafAST) extends Exception with SemanticResult{
  override def toString = message +" in line "+ node.pos.line.toString
}
