package gui

import scala.swing._
import panels.{LeftPanel,RightPanel}
import au.ken.treeview._

object Main extends SimpleSwingApplication{
  def top = new MainFrame{
    title = "kDecaf!"

    val input = io.Source.fromFile("/home/kmels/decaf").mkString
    /*val input = """class Program {
	int a;
    }    
    """*/
    import util.parsing.combinator.{lexical,syntactical,Parsers}
    import syntactical._
    import lexical._
    import parsing._
    import parsing.ast._

    val result = new KDecafParser() parse(input)
    val tree = new Tree[KDecafAST](result.get,_.children)
    contents = tree
    
    contents = new FlowPanel {
      contents += LeftPanel
      contents += RightPanel
    }

    peer.setResizable(false)
  }
}
