enablePlugins(ScalaJSPlugin, EsbuildPlugin)

name := "nostr-army-knife"
scalaVersion := "3.3.4"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "com.armanbilge" %%% "calico" % "0.2.0-RC2",
      "io.github.vzxplnhqr" %%% "snow" % "0.0.5"
    ),
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }
  )
