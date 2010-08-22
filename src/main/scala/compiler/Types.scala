package compiler.types
import compiler.parsing.ast.KDecafAST

package object aliases {
  type Scope = String //scope name
  type Symbol = (String,Scope) //symbol name, scope
  type Attribute = KDecafAST   
}

import aliases._

case class AttributeList(l:List[Attribute]) extends Attribute{
  val children = Nil
}
