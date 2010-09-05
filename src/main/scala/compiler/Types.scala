package kmels.uvg.kdecaf.compiler.types

import kmels.uvg.kdecaf.compiler.parsing.ast.Node

package object aliases {
  type Scope = String //scope name
  type Symbol = (String,Scope) //symbol name, scope
  type SemanticErrorMessage = (String,Int,Int) //message,line,column
}

