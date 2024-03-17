import org.scalajs.linker.interface.ModuleSplitStyle

val scala3Version = "3.3.1"

lazy val root = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .settings(
    name := "DDNetTools",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.3.0",
      "org.scalameta" %%% "munit" % "1.0.0-M11" % Test
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