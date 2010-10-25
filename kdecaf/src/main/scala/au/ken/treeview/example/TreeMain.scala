package au.ken.treeview.example

import scala.swing._
import scala.swing.event._
//import Swing._
//import javax.swing.Icon
import au.ken.treeview._
import au.ken.treeview.event._


object TreeMain {//extends SimpleSwingApplication {
  import Tree._
  import java.io._
  import java.awt.Color

  case class Person(name: String, subordinates: List[Person])
  val persons = Person("John", List(Person("Jack", Nil), Person("Jill", List(Person("Betty", Nil)))))
  
  val tree = new Tree[Person](persons, _.subordinates) { 
    //editable = true
    renderer = Renderer(_.name)
    editor = Editor[Person, String](_.name, s => Person(s, Nil))
  }
  
 /* class CheckBoxRenderer[A](convert: A => (Icon, Boolean)) extends Tree.AbstractRenderer[A, CheckBox](new CheckBox) {
    def this() = this(a => (null, Option(a).isDefined))
   
    override def configure(tree: Tree[_], a: A, p: Renderer.Params) {
      val (icon, selected) = convert(a)
      component.icon = icon
      component.selected = selected
    }
  }*/
  
 /* val javaTree = new javax.swing.JTree(Array("1", "2", "3", "4"): Array[Object]) {
    import javax.swing.tree._
    setCellEditor(new DefaultTreeCellEditor(this, new DefaultTreeCellRenderer()))
    setEditable(true)
  }*/  
  def top = new MainFrame {
    contents = tree
    listenTo(tree.editor, tree.selection, tree.mouse.clicks)
      
    reactions += {
      case TreePathSelected(_, _, _, newSelection, _) => 
          val last = for (p <- newSelection; last <- p.lastOption) yield last
          println(last getOrElse "None found")
          
      case MouseClicked(tree, _, _, 2, _) => println("click!")
      
      case CellEditingStopped(source: CellEditor[_]) => println(source.value)
    }
  }

  /*def xmlSeqTree = new Tree[Node](
    <html>
      <body>
        <div id="title">Yurtle the Turtle</div>
        <div id="description">This is great for the kids.</div>
      </body>
    </html>, {_.child filterNot (_.text.trim.isEmpty)}) {
      
    renderer = Renderer(n => if (n.label.startsWith("#")) n.text.trim else n.label)
  }
  
  def files(f: File) = if (f.isDirectory) f.listFiles.toSeq else Seq.empty
  
  def filteredFiles(filter: File => Boolean)(f: File): Seq[File] = {
      if (f.isDirectory) f.listFiles(new FileFilter {
        def accept(file: File) = filter(file)
      }) 
      else Seq.empty
  }
  
  def products(node: Any) = node match {
    case s: Seq[_] => s
    case o => Seq.empty
  }
  
  def seqs(node: Any) = node match {
    case s: Seq[_] => s
    case o => Seq.empty
  }
  
  def nestedCaseClassesTree = {
    sealed trait Expr
    case class Plus(a: Expr, b: Expr) extends Expr {override def toString() = a + " + " + b}
    case class Minus(a: Expr, b: Expr) extends Expr {override def toString() = a + " - " + b}
    case class Const(value: Int) extends Expr {override def toString() = value.toString}
    
    new Tree(Plus(Const(5), Minus(Const(2), Const(1))), products)
  }
  /*
  def nestedTupleTree = new Tree(Titled("Foods", (Titled("Legumes", ("Lentils", "Chick peas")), "Fruit", "Chips")), products)
  def nestedListTree = new Tree(Titled("Numbers", 1 :: 2 :: 3 :: Titled("Them", (4 :: 5 :: 6 :: Nil)) :: Nil), products)
  
  def nestedSeqTree = new Tree(Titled("Numbers", Seq(1, 2, 3, Titled("Them", Seq(4, 5, 6)))), seqs)*/
  def infiniteFactorTree = Tree(10000) {n => 1 to n filter (n % _ == 0)}
  def fileTree = new Tree(new File("."), files)
  def filteredFileTree = new Tree(new File("."), filteredFiles(f => f.isDirectory || f.getName.endsWith(".scala")))*/
}
