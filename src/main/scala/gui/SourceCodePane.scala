package gui.panels

import scala.swing.{EditorPane,ScrollPane}

object SourceCodeEditor extends EditorPane("text/plain","//Your decaf program") {
  peer.setPreferredSize(new java.awt.Dimension(800,600))
}

object SourceCodePane extends ScrollPane(SourceCodeEditor)

