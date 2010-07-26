package gui.panels

import scala.swing.ScrollPane
import au.ken.treeview.Tree
import parsing.ast.KDecafAST

class TreePane[T](val root:T, val children: T => Seq[T]) extends ScrollPane(new Tree[T](root,children))
