name := "strategygames"

organization := "org.playstrategy"

version := "10.2.1-pstrat43"

scalaVersion := "2.13.5"

val fairystockfishVersion = "0.0.7"

libraryDependencies ++= List(
  "org.scala-lang.modules"   %% "scala-parser-combinators" % "1.1.2",
  "org.specs2"               %% "specs2-core"              % "4.10.0" % Test,
  "org.specs2"               %% "specs2-cats"              % "4.10.0" % Test,
  "com.github.ornicar"       %% "scalalib"                 % "7.0.2",
  "joda-time"                 % "joda-time"                % "2.10.10",
  "org.typelevel"            %% "cats-core"                % "2.2.0",
  "org.playstrategy"          % "fairystockfish"           % fairystockfishVersion,
  "com.joansala.aalina"       % "aalina"                   % "2.1.0-pstrat1"
)

// Explicitly add in the linux-class path
lazy val fairystockfish = Artifact("fairystockfish", "linux-x86_64")
libraryDependencies += "org.playstrategy" % "fairystockfish" % fairystockfishVersion artifacts fairystockfish
classpathTypes ++= Set("linux-x86_64")

resolvers ++= Seq(
  "lila-maven" at "https://raw.githubusercontent.com/Mind-Sports-Games/lila-maven/master",
 ) ++ sys.env.get("LILA_MAVEN_RESOLVERS").map(_.split(",").zipWithIndex.map{case (x,i) => s"local-maven-$i" at x}).map(_.toSeq).getOrElse(Seq())


scalacOptions ++= Seq(
  "-encoding",
  "utf-8",
  "-explaintypes",
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:postfixOps",
  "-Ymacro-annotations",
  // Warnings as errors!
  // "-Xfatal-warnings",
  // Linting options
  "-unchecked",
  "-Xcheckinit",
  "-Xlint:adapted-args",
  "-Xlint:constant",
  "-Xlint:delayedinit-select",
  "-Xlint:deprecation",
  "-Xlint:inaccessible",
  "-Xlint:infer-any",
  "-Xlint:missing-interpolator",
  "-Xlint:nullary-unit",
  "-Xlint:option-implicit",
  "-Xlint:package-object-classes",
  "-Xlint:poly-implicit-overload",
  "-Xlint:private-shadow",
  "-Xlint:stars-align",
  "-Xlint:type-parameter-shadow",
  "-Wdead-code",
  "-Wextra-implicit",
  // "-Wnumeric-widen",
  "-Wunused:imports",
  "-Wunused:locals",
  "-Wunused:patvars",
  "-Wunused:privates",
  "-Wunused:implicits",
  "-Wunused:params",
  "-Wvalue-discard",
  "-Xmaxerrs",
  "12"
)

publishTo := Option(Resolver.file("file", new File(sys.props.getOrElse("publishTo", ""))))
