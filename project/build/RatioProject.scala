import sbt._
import java.io.File

class RatioProject(info: ProjectInfo) extends DefaultProject(info) {

  override def compileOptions = Optimize :: Unchecked :: super.compileOptions.toList

  override def repositories = Set(ScalaToolsSnapshots,
                                  "Fyrie Releases" at "http://repo.fyrie.net/releases/",
                                  "Fyrie Snapshots" at "http://repo.fyrie.net/snapshots/")

  override def managedStyle = ManagedStyle.Maven
  val publishUser = "derek"
  val publishKeyFile = new java.io.File("/home/derek/.ssh/id_rsa")
  val publishTo = projectVersion.value match {
    case BasicVersion(_,_,_,Some("SNAPSHOT")) =>
      Resolver.sftp("Fyrie Snapshots (publish)", "repo.fyrie.net", "/home/repo/snapshots") as(publishUser, publishKeyFile)
    case _ =>
      Resolver.sftp("Fyrie Releases (publish)", "repo.fyrie.net", "/home/repo/releases") as(publishUser, publishKeyFile)
  }

  val specs = "org.scala-tools.testing" %% "specs" % "1.6.5" % "test"
  val scalacheck = "org.scala-tools.testing" %% "scalacheck" % "1.7" % "test"

}

