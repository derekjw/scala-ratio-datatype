import sbt._

class RatioProject(info: ProjectInfo) extends DefaultProject(info) {

  override def compileOptions = Optimize :: Unchecked :: super.compileOptions.toList

  override def repositories = Set(ScalaToolsSnapshots,
                                  "Fyrie Releases" at "http://repo.fyrie.net/releases/",
                                  "Fyrie Snapshots" at "http://repo.fyrie.net/snapshots/")

  val specs = "org.scala-tools.testing" %% "specs" % "1.6.5-SNAPSHOT" % "test"
  val scalacheck = "org.scala-tools.testing" %% "scalacheck" % "1.7" % "test->default"

}

