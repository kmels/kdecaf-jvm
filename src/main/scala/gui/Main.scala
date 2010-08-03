package gui

import scala.swing._
import swing.event._
import panels.{LeftPanel,RightPanel}

object Main extends SimpleSwingApplication{
  def top = new MainFrame{
    title = "kDecaf!"
    
    contents = new FlowPanel {
      contents += LeftPanel
      contents += RightPanel
    }

    peer.setResizable(false)
  }
}
