val scala3Version = "3.8.4"

lazy val root = project
    .in(file("."))
    .settings(
      name := "kaos-dsl",
      version := "0.1.0-SNAPSHOT",

      scalaVersion := scala3Version,

      libraryDependencies += "org.scalameta" %% "munit" % "1.3.4" % Test,

      libraryDependencies += "org.parboiled" %% "parboiled" % "2.5.1",

      Compile / unmanagedSourceDirectories +=
          baseDirectory.value / "visualizer"
    )
