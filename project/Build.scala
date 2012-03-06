import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "rocky"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
        "org.mockito" % "mockito-core" % "1.9.0"
    )

    val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
    )

}
