import sbt._ 
 
class Project(info: ProjectInfo) extends DefaultProject(info) { 
  val scalaToolsSnapshots = ScalaToolsSnapshots
  val scalatest = "org.scalatest" % "scalatest" %
    "1.2"

  override def compileOptions = super.compileOptions ++Seq(Unchecked)
}
