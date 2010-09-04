package kmels.uvg.kdecaf.gui.panels

import scala.swing.{BoxPanel,Orientation,Button,Component}
import java.awt.Dimension
import swing.event.ButtonClicked
import kmels.uvg.kdecaf.compiler
import compiler.parsing.KDecafParser
import compiler.parsing.ast.{Node,Program}

object LeftPanel extends BoxPanel(Orientation.Vertical){
  object importFileButton extends Button("Import file"){
    peer.setPreferredSize(new Dimension(50,50))
  }

  object runButton extends Button{
    peer.setPreferredSize(new Dimension(50,50))
    text = "Run"
  }

  object compileButton extends Button("Compile"){
    peer.setPreferredSize(new Dimension(50,50))
  }

  contents += importFileButton
//  contents += runButton
  contents += compileButton
  
  listenTo(importFileButton)
  listenTo(compileButton)


  case class Person(name: String, subordinates: List[Person]){
    override def toString = name
  }
  val otro = Person("otro",Nil)
  val john = Person("John",List.fill(4)(otro))
  val persons = Person("John",List.fill(25)(john))
  val input = scala.io.Source.fromFile("/home/kmels/decaf").mkString
  val result = new KDecafParser() parse(input)
  val tree:Program = result.get
  //contents += new TreePane[KDecafAST](tree,_.children)
  contents += new TreePane[Person](persons ,x => x.subordinates)

  reactions += {
    case ButtonClicked(`importFileButton`) => {
      val pathToFile = "/home/kmels/decaf"
      SourceCodeEditor.peer.setText(scala.io.Source.fromFile(pathToFile).mkString)
    }
    case ButtonClicked(`compileButton`) => {
      val input = SourceCodeEditor.peer.getText
      val result = new KDecafParser() parse(input)
      
      if (result.successful){
	val tree:Program = result.get
	val newTree = new TreePane[Node](tree,_.children)
	val model:javax.swing.tree.TreeModel = newTree.contents(0).peer.asInstanceOf[javax.swing.JTree].getModel
	contents(2).asInstanceOf[TreePane[_]].contents(0).peer.asInstanceOf[javax.swing.JTree].setModel(model)
      }else{
	println("Failed")
      }	
    }
  }
  
  peer.setPreferredSize(new java.awt.Dimension(340,680))
}

object RightPanel extends BoxPanel(Orientation.Vertical){
  contents += ConsolePane
  contents += SourceCodePane
}
