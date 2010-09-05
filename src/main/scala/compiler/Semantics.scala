package kmels.uvg.kdecaf.compiler.semantics

//import kmels.uvg.kdecaf.compiler.{SymbolAttribute,SymbolAttributes,SymbolAttributes2}
import kmels.uvg.kdecaf.compiler
import compiler.parsing.ast.{Node,InnerType}
import compiler.types.{aliases => typeAliases}
import typeAliases._

/**
 * Semantic rules
 *
 * @author Carlos Lopez
 * @version 1.0
 * @since 2.0
 */
trait SemanticRule{
  self: Node =>
    
  import typeAliases.Scope

  val typesShouldBeEqualIn: (InnerType,InnerType,String) => SemanticResult = (node1,node2,message) => 
    if (node1.getUnderlyingType() == node2.getUnderlyingType()) 
      SemanticSuccess 
    else 
      SemanticError(message)

  def SemanticAction(f: Scope => SemanticResult) = new SemanticAction{
    def apply(attributes: Scope) = f(attributes)
  }

  def SemanticError(message: String) = new SemanticError((message,this))

  val semanticAction:SemanticAction
}

trait NoSemanticAction extends SemanticRule{
  self: Node => 
  val semanticAction = SemanticAction(
    attributes => SemanticError("pendiente")
  )
}
trait SemanticResult

object SemanticSuccess extends SemanticResult

case class SemanticResults(val results:SemanticResult*) extends SemanticResult

abstract class SemanticAction extends (Scope => SemanticResult)

case class SemanticError(val errors:SemanticErrorMessage*) extends SemanticResult{
  //val messages = errors.map
}
