package gui.panels

import scala.swing.{TextArea,ScrollPane,TabbedPane}
/**
 * The console, where the I/O of executing programs is done.
 */ 
object Konsole extends TextArea{
  peer.setPreferredSize(new java.awt.Dimension(800,80))
}

object ConsolePane extends ScrollPane(Konsole)
