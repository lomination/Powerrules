import org.scalajs.linker.interface.ModuleSplitStyle

val scala3Version = "3.3.6"

lazy val root = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .enablePlugins(BuildInfoPlugin)
  .settings(
    name         := "Powerrules",
    version      := "0.5.1-SNAPSHOT",
    scalaVersion := scala3Version,
    scalacOptions ++= Seq(
      "-encoding", "utf8",
      "-Xfatal-warnings",
      "-deprecation",
      "-feature",
      "-unchecked"
    ),
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.4.0",
      "org.log4s"              %%% "log4s"                    % "1.10.0",
      "org.scalameta"          %%% "munit"                    % "1.0.0-M3" % Test
    ),
    buildInfoPackage := "lomination.powerrules.build",
    buildInfoKeys ++= Seq[BuildInfoKey](
      version
    )
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "ch.qos.logback" % "logback-classic" % "1.2.10"
    )
  )
  .jsSettings(
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
        .withModuleSplitStyle(ModuleSplitStyle.SmallModulesFor(List("powerrules")))
    },
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "2.4.0",
      "com.lihaoyi"  %%% "scalatags"   % "0.12.0"
    )
  )
