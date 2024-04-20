import org.scalajs.linker.interface.ModuleSplitStyle

val scala3Version = "3.3.1"

lazy val root = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name         := "DDNetTools",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    scalacOptions ++= Seq(
      "-encoding", "utf8",
      "-Xfatal-warnings",
      "-deprecation",
      "-feature",
      "-unchecked"
    ),
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.3.0",
      "org.log4s"              %%% "log4s"                    % "1.10.0",
      "org.scalameta"          %%% "munit"                    % "1.0.0-M11" % Test
    ),
    buildInfoPackage := "lomination.ddnettools.build",
    buildInfoKeys ++= Seq[BuildInfoKey](
      version,
      scalaVersion,
      BuildInfoKey.apply(
        "gitDescription", {
          import scala.sys.process._
          "git describe --match=Nothing --always --abbrev=9 --dirty".!!.trim
        }
      )
    )
  )
  .jsSettings(
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(ModuleSplitStyle.SmallModulesFor(List("ddnettools")))
    },
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "2.4.0",
      "com.lihaoyi"  %%% "scalatags"   % "0.12.0"
    )
  )
