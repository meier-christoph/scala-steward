addSbtPlugin("com.codecommit" % "sbt-github-actions" % "0.9.5")
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.15.0")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.10.0")
addSbtPlugin("com.geirsson" % "sbt-ci-release" % "1.5.4")
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.2")
addSbtPlugin("com.github.cb372" % "sbt-explicit-dependencies" % "0.2.15")
addSbtPlugin("com.github.tkawachi" % "sbt-doctest" % "0.9.8")
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.7.6")
addSbtPlugin("de.heikoseeberger" % "sbt-header" % "5.6.0")
addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat" % "0.1.15")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.0.0")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.6.1")
addSbtPlugin("org.wartremover" % "sbt-wartremover" % "2.4.13")

// This is only here so that Scala Steward updates the version in sbt/package.scala too.
addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.9.23")

// workaround for sbt-doctest throwing java.lang.NoClassDefFoundError: org/apache/commons/io/FilenameUtils
libraryDependencies ++= Seq(
  "commons-io" % "commons-io" % "2.8.0"
)
