enablePlugins(ScalaJSPlugin, EsbuildPlugin)

name := "nostr-army-knife"
scalaVersion := "3.3.4"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "com.armanbilge" %%% "calico" % "0.2.3",
      "com.armanbilge" %%% "calico-router" % "0.2.3",
      "io.github.vzxplnhqr" %%% "snow" % "0.0.8"
    ),
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }
  )
