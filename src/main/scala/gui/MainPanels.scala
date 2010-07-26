package gui.panels

import scala.swing.{BoxPanel,Orientation,Button,Component}
import java.awt.Dimension

object LeftPanel extends BoxPanel(Orientation.Vertical){
  object runButton extends Button{
    peer.setPreferredSize(new Dimension(50,50))
    text = "Run"
  }

  val compileButton = new Button("Compile"){
    peer.setPreferredSize(new Dimension(50,50))
  }

  contents += runButton
  contents += compileButton

  case class Person(name: String, subordinates: List[Person]){
    override def toString = name
  }
  val john = Person("John",Nil)
  val persons = Person("John",List.fill(25)(john))
  contents += new TreePane[Person](persons ,x => x.subordinates)
  peer.setPreferredSize(new java.awt.Dimension(260,680))
}

object RightPanel extends BoxPanel(Orientation.Vertical){
  contents += ConsolePane
  contents += SourceCodePane
}
