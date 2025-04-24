package strategygames.abalone.opening

// format: off
object EcopeningDB {

  import Ecopening._

  val MAX_TURNS = 25

  lazy val all = allByEco.values.toList.sorted

  lazy val allByFen: Map[FEN, Ecopening] = allByEco.map {
    case (_, opening) => opening.fen -> opening
  }

  //TODO Alex reverse FEN
  lazy val allByEco: Map[ECO, Ecopening] = Map(
"A00" -> new Ecopening("A00", "abalone", "Abalone Belgian Daisy Start Pos", "Abalone Belgian Daisy Start Pos", "", "ss1SS/sssSSS/1ss1SS1/8/9/8/1SS1ss1/SSSsss/SS1ss", ""),
"A01" -> new Ecopening("A01", "abalone", "Abalone Scorpio opening", "Abalone Scorpio opening", "a1d4", "ss1SS/sssSSS/1ss1SS1/8/9/3S4/1SS1ss1/SSSsss/1S1ss", "a1d4"),
"A02" -> new Ecopening("A02", "abalone", "Abalone double Scorpio opening", "Abalone double Scorpio opening", "a1d4 e9e6", "1s1SS/sssSSS/1ss1SS1/3s4/9/3S4/1SS1ss1/SSSsss/1S1ss", "e9e6"),
"A03" -> new Ecopening("A03", "abalone", "Abalone Rabbit Scorpio opening", "Abalone Rabbit Scorpio opening", "a1d4 e9e6 b1d3", "1s1SS/sssSSS/1ss1SS1/3s4/9/3S4/1SSSss1/SSSsss/3ss", "b1d3"),
"A04" -> new Ecopening("A04", "abalone", "Abalone double Rabbit opening", "Abalone double Rabbit opening", "a1d4 e9e6 b1d3 f9f7", "3SS/sssSSS/1sssSS1/3s4/9/3S4/1SSSss1/SSSsss/3ss", "f9f7"),
"A05" -> new Ecopening("A05", "abalone", "Abalone Bat Rabbit opening", "Abalone Bat Rabbit opening", "a1d4 e9e6 b1d3 f9f7 c2e4", "3SS/sssSSS/1sssSS1/3s4/9/3SS3/1SSSss1/SS1sss/3ss", "c2e4"),
"A06" -> new Ecopening("A06", "abalone", "Abalone Bat Rabbit", "Abalone Bat Rabbit", "a1d4 e9e6 b1d3 f9f7 c2e4 f2g3", "3SS/sssSSS/1sssSS1/3s4/9/3SS3/1SSSsss/SS1ss1/3ss", "f2g3"),
"A07" -> new Ecopening("A07", "abalone", "Abalone Bat Rabbit", "Abalone Bat Rabbit", "a1d4 e9e6 b1d3 f9f7 c2e4 f2g3 i8i7", "3SS/sssSS1/1sssSSS/3s4/9/3SS3/1SSSsss/SS1ss1/3ss", "i8i7"),
"B01" -> new Ecopening("B01", "abalone", "Abalone Scorpio opening - reversed", "Abalone Scorpio opening - reversed", "i9f6", "ss1S1/sssSSS/1ss1SS1/4S3/9/8/1SS1ss1/SSSsss/SS1ss", "i9f6"),
"B02" -> new Ecopening("B02", "abalone", "Abalone double Scorpio opening - reversed", "Abalone double Scorpio opening - reversed", "i9f6 e1e4", "ss1S1/sssSSS/1ss1SS1/4S3/9/4s3/1SS1ss1/SSSsss/SS1s1", "e1e4"),
"B03" -> new Ecopening("B03", "abalone", "Abalone Rabbit scorpio opening - reversed", "Abalone Rabbit scorpio opening - reversed", "i9f6 e1e4 h9f7", "ss3/sssSSS/1ssSSS1/4S3/9/4s3/1SS1ss1/SSSsss/SS1s1", "h9f7"),
"B04" -> new Ecopening("B04", "abalone", "Abalone double Rabbit opening - reversed", "Abalone double Rabbit opening - reversed", "i9f6 e1e4 h9f7 d1d3", "ss3/sssSSS/1ssSSS1/4S3/9/4s3/1SSsss1/SSSsss/SS3", "d1d3"),
"B05" -> new Ecopening("B05", "abalone", "Abalone Bat Rabbit opening - reversed", "Abalone Bat Rabbit opening - reversed", "i9f6 e1e4 h9f7 d1d3 g8e6", "ss3/sss1SS/1ssSSS1/3SS3/9/4s3/1SSsss1/SSSsss/SS3", "g8e6"),
"B06" -> new Ecopening("B06", "abalone", "Abalone Bat Rabbit - reversed", "Abalone Bat Rabbit - reversed", "i9f6 e1e4 h9f7 d1d3 g8e6 d8c7", "ss3/1ss1SS/sssSSS1/3SS3/9/4s3/1SSsss1/SSSsss/SS3", "d8c7"),
"B07" -> new Ecopening("B07", "abalone", "Abalone Bat Rabbit - reversed", "Abalone Bat Rabbit - reversed", "i9f6 e1e4 h9f7 d1d3 g8e6 d8c7 a2a3", "ss3/1ss1SS/sssSSS1/3SS3/9/4s3/SSSsss1/1SSsss/SS3", "a2a3"),
  )
}
