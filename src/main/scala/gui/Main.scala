package gui

import scala.swing._

object Main extends SimpleSwingApplication{
  def top = new MainFrame{
    title = "PTWB"

    contents = new FlowPanel{
      
      border = Swing.EmptyBorder(30,30,10,30)
    }
  }
}
