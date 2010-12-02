import sbt._ 
import de.element34.sbteclipsify._

class Project(info: ProjectInfo) extends DefaultProject(info) with Eclipsify{ 
  val scalaToolsSnapshots = ScalaToolsSnapshots
  val scalatest = "org.scalatest" % "scalatest" %
    "1.2"
 
  override def compileOptions = super.compileOptions ++Seq(Unchecked)  
}
