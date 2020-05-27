// Used by VersionUtil to get gitHash and commitDate
libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit" % "4.11.0.201803080745-r"

// Include the sources of the sbt-dotty plugin in the project build,
// so that we can use the current in-development version of the plugin
// in our build instead of a released version.

unmanagedSourceDirectories in Compile += {
  val newPluginRequired = ">=1.4.0-SNAPSHOT"
  val root = baseDirectory.value
  val useNew = VersionNumber(sbtVersion.value).matchesSemVer(SemanticSelector(newPluginRequired))
  val dir = if (useNew) "sbt-dotty-new" else "sbt-dotty"
  root / s"../$dir/src"
}

// Keep in sync with `sbt-dotty` config in Build.scala
libraryDependencies ++= Seq(
  Dependencies.`jackson-databind`,
  Dependencies.`compiler-interface`
)
unmanagedSourceDirectories in Compile +=
  baseDirectory.value / "../language-server/src/dotty/tools/languageserver/config"
