package gui.panels

import scala.swing.{BoxPanel,Orientation,Button,Component}
import java.awt.Dimension

object LeftPanel extends BoxPanel(Orientation.Vertical){
  object runButton extends Button{
    peer.setPreferredSize(new Dimension(80,80))
    text = "Run"
  }

  val compileButton = new Button("Compile")

  contents += runButton
  contents += compileButton

}

object RightPanel extends BoxPanel(Orientation.Vertical){
  contents += ConsolePane
  contents += SourceCodePane
}
