enablePlugins(ScalaJSPlugin, EsbuildPlugin)

name := "nostr-army-knife"
scalaVersion := "3.3.0-RC4"

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "com.armanbilge" %%% "calico" % "0.2.0-RC2",
      "com.fiatjaf" %%% "snow" % "0.0.1"
    ),
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }
  )
