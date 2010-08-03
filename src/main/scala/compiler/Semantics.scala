package compiler.semantics

/**
 * Semantic rules
 *
 * @author Carlos Lopez
 * @version 1.0
 * @since 2.0
 */
trait SemanticRule{
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


