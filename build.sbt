name := "strategygames"

organization := "org.playstrategy"

version := "10.2.1-pstrat202"

scalaVersion := "3.7.4"

val fairystockfishVersion = "0.0.20"
val scalalibVersion = "11.9.5"

libraryDependencies ++= List(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
  "org.specs2"             %% "specs2-core"              % "5.7.0" % Test,
  "org.specs2"             %% "specs2-cats"              % "4.23.0" % Test,
  "joda-time"               % "joda-time"                % "2.10.10",
  "org.typelevel"          %% "cats-core"                % "2.13.0",
  "org.playstrategy"        % "fairystockfish"           % fairystockfishVersion,
  "com.joansala.aalina"     % "aalina"                   % "2.1.0-pstrat2",
  "com.joansala"            % "go-engine"                % "1.0.0-pstrat1.12",
  "com.github.lichess-org.scalalib" %% "scalalib-core" % "11.9.5"
)

// Explicitly add in the linux-class path
lazy val fairystockfish = Artifact("fairystockfish", "linux-x86_64")
libraryDependencies += "org.playstrategy" % "fairystockfish" % fairystockfishVersion artifacts fairystockfish
classpathTypes ++= Set("linux-x86_64")

resolvers ++= Seq(
  "lila-maven".at("https://raw.githubusercontent.com/Mind-Sports-Games/lila-maven/master"),
  "jitpack".at("https://jitpack.io")
) ++ sys.env
  .get("LILA_MAVEN_RESOLVERS")
  .map(_.split(",").zipWithIndex.map { case (x, i) => s"local-maven-$i" at x })
  .map(_.toSeq)
  .getOrElse(Seq())

scalacOptions ++= Seq(
  "-encoding",
  "utf-8",
  "-explain",
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-unchecked",
  "-Wunused:patvars",
  "-Wunused:implicits",
  "-Wunused:params",
  "-source:3.0-migration"
)

Compile / packageDoc / publishArtifact := false

publishTo := Option(Resolver.file("file", new File(sys.props.getOrElse("publishTo", ""))))
