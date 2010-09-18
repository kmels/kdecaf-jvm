package kmels.uvg.kdecaf.gui.panels

import scala.swing.ScrollPane
import au.ken.treeview.Tree
import kmels.uvg.kdecaf.compiler.parsing.ast.Node

class TreePane[T](val root:T, val children: T => Seq[T]) extends ScrollPane(new Tree[T](root,children))

object TreePane extends TreePane[Any](None,None => Seq())
