import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "rocky"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
        "org.mockito" % "mockito-core" % "1.9.0",
        "com.nowanswers" %% "chemistry" % "0.1.1"
    )

    val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(

      resolvers ++= Seq(
        Resolver.file("Local ivy Repository", file("/Users/ladlestein/.ivy2/local/"))(Resolver.ivyStylePatterns)
      )


    )

}
