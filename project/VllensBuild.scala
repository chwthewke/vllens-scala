import sbt._
import sbt.Keys._
import com.typesafe.sbt.SbtScalariform._
import com.typesafe.sbteclipse.core.EclipsePlugin._
import scalariform.formatter.preferences._
import scoverage.ScoverageSbtPlugin
import sbtbuildinfo.Plugin._

object VllensBuild extends Build {

  object Dependencies {
    val scalatest = "org.scalatest" %% "scalatest" % "3.0.0-M15" % "test" withSources () withJavadoc ()

    val scalacheck = Seq(
      "org.scalacheck" %% "scalacheck" % "1.13.0" % "test" withSources () withJavadoc ()
    )

    val cats = "org.typelevel" %% "cats" % "0.4.1" withSources () withJavadoc ()
  }

  override def settings = super.settings :+ ( EclipseKeys.skipParents in ThisBuild := false )

  lazy val vllensScalariformSettings = scalariformSettings ++ Seq(
    ScalariformKeys.preferences := defaultPreferences
      .setPreference( AlignSingleLineCaseStatements, true )
      .setPreference( SpaceBeforeColon, true )
      .setPreference( SpaceInsideParentheses, true )    
  )

  lazy val sharedSettings =
    Seq(
      organization := "net.chwthewke",
      scalaVersion := "2.11.8")

  lazy val vllensSettings = 
    Defaults.coreDefaultSettings ++
    SbtBuildInfo.buildSettings("net.chwthewke.vllens") ++
    SbtEclipse.buildSettings ++
    vllensScalariformSettings ++
    sharedSettings ++
    Seq(
      libraryDependencies ++= Seq(
          Dependencies.scalatest,
          Dependencies.cats ) ++
          Dependencies.scalacheck,
        scalacOptions ++= Seq(
          "-feature",
          "-deprecation",
          "-language:higherKinds" ),
        unmanagedSourceDirectories in Compile := (scalaSource in Compile).value :: Nil,
        unmanagedSourceDirectories in Test := (scalaSource in Test).value :: Nil
    )

  lazy val vllens = Project(
    id = "vllens",
    base = file( "." ),
    settings = vllensSettings ++
      Seq(
        name := "vllens",
        mainClass := Some("net.chwthewke.vllens.Main"),
        initialCommands := """|import net.chwthewke.vllens._
                              |import scalaz._,Scalaz._""".stripMargin
      )
  )
}
