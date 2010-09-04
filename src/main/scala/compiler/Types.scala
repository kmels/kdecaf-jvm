package kmels.uvg.kdecaf.compiler.types

import kmels.uvg.kdecaf.compiler.parsing.ast.Node

package object aliases {
  type Scope = String //scope name
  type Symbol = (String,Scope) //symbol name, scope
  type Attribute = Node
}

import aliases._

case class AttributeList(l:List[Attribute]) extends Attribute{
  val children = Nil
}
