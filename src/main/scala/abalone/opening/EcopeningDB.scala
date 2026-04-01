package strategygames.abalone.opening

// format: off
object EcopeningDB {

  import Ecopening._

  val MAX_TURNS = 25

  lazy val all = allByEco.values.toList.sorted

  lazy val allByFen: Map[FEN, Ecopening] = allByEco.map {
    case (_, opening) => opening.fen -> opening
  }

  lazy val allByEco: Map[ECO, Ecopening] = Map(
"A00" -> new Ecopening("A00", "abalone", "Belgian Daisy", "Starting Position", "", "ss1SS/sssSSS/1ss1SS1/8/9/8/1SS1ss1/SSSsss/SS1ss", ""),
"A01" -> new Ecopening("A01", "abalone", "Belgian Daisy", "Scorpio", "a1d4", "ss1SS/sssSSS/1ss1SS1/8/9/3S4/1SS1ss1/SSSsss/1S1ss", "a1d4"),
"A02" -> new Ecopening("A02", "abalone", "Belgian Daisy", "Double Scorpio", "a1d4 i5f5", "1s1SS/sssSSS/1ss1SS1/3s4/9/3S4/1SS1ss1/SSSsss/1S1ss", "i5f5"),
"A03" -> new Ecopening("A03", "abalone", "Belgian Daisy", "Rabbit Scorpio", "a1d4 i5f5 a2c4", "1s1SS/sssSSS/1ss1SS1/3s4/9/3S4/1SSSss1/SSSsss/3ss", "a2c4"),
"A04" -> new Ecopening("A04", "abalone", "Belgian Daisy", "Double Rabbit", "a1d4 i5f5 a2c4 i6g6", "3SS/sssSSS/1sssSS1/3s4/9/3S4/1SSSss1/SSSsss/3ss", "i6g6"),
"A05" -> new Ecopening("A05", "abalone", "Belgian Daisy", "Bat Rabbit", "a1d4 i5f5 a2c4 i6g6 b3d5", "3SS/sssSSS/1sssSS1/3s4/9/3SS3/1SSSss1/SS1sss/3ss", "b3d5"),
"A06" -> new Ecopening("A06", "abalone", "Belgian Daisy", "Bat Rabbit", "a1d4 i5f5 a2c4 i6g6 b3d5 b6c7", "3SS/sssSSS/1sssSS1/3s4/9/3SS3/1SSSsss/SS1ss1/3ss", "b6c7"),
"A07" -> new Ecopening("A07", "abalone", "Belgian Daisy", "Bat Rabbit", "a1d4 i5f5 a2c4 i6g6 b3d5 b6c7 h9g9", "3SS/sssSS1/1sssSSS/3s4/9/3SS3/1SSSsss/SS1ss1/3ss", "h9g9"),
"AR01" -> new Ecopening("AR01", "abalone", "Belgian Daisy", "Scorpio - R", "i9f6", "ss1S1/sssSSS/1ss1SS1/4S3/9/8/1SS1ss1/SSSsss/SS1ss", "i9f6"),
"AR02" -> new Ecopening("AR02", "abalone", "Belgian Daisy", "Double Scorpio - R", "i9f6 a5d5", "ss1S1/sssSSS/1ss1SS1/4S3/9/4s3/1SS1ss1/SSSsss/SS1s1", "a5d5"),
"AR03" -> new Ecopening("AR03", "abalone", "Belgian Daisy", "Rabbit Scorpio - R", "i9f6 a5d5 i8g6", "ss3/sssSSS/1ssSSS1/4S3/9/4s3/1SS1ss1/SSSsss/SS1s1", "i8g6"),
"AR04" -> new Ecopening("AR04", "abalone", "Belgian Daisy", "Double Rabbit - R", "i9f6 a5d5 i8g6 a4c4", "ss3/sssSSS/1ssSSS1/4S3/9/4s3/1SSsss1/SSSsss/SS3", "a4c4"),
"AR05" -> new Ecopening("AR05", "abalone", "Belgian Daisy", "Bat Rabbit - R", "i9f6 a5d5 i8g6 a4c4 h7f5", "ss3/sss1SS/1ssSSS1/3SS3/9/4s3/1SSsss1/SSSsss/SS3", "h7f5"),
"AR06" -> new Ecopening("AR06", "abalone", "Belgian Daisy", "Bat Rabbit - R", "i9f6 a5d5 i8g6 a4c4 h7f5 h4g3", "ss3/1ss1SS/sssSSS1/3SS3/9/4s3/1SSsss1/SSSsss/SS3", "h4g3"),
"AR07" -> new Ecopening("AR07", "abalone", "Belgian Daisy", "Bat Rabbit - R", "i9f6 a5d5 i8g6 a4c4 h7f5 h4g3 b1c1", "ss3/1ss1SS/sssSSS1/3SS3/9/4s3/SSSsss1/1SSsss/SS3", "b1c1"),
// Grand Abalone openings (11x11 board, variantGrouping = "grandabalone")
"GA-A00" -> new Ecopening("GA-A00", "grandabalone", "Grand Belgian Daisy", "Start Pos", "", "SS2ss/SSS1sss/1SS2ss1/9/ss6SS/sss5SSS/ss6SS/9/1SS2ss1/SSS1sss/SS2ss", ""),
"GA-A01" -> new Ecopening("GA-A01", "grandabalone", "Grand Belgian Daisy", "Scorpio", "a1d4", "SS2ss/SSS1sss/1SS2ss1/9/ss6SS/sss5SSS/ss6SS/3S5/1SS2ss1/SSS1sss/1S2ss", "a1d4"),
"GA-A02" -> new Ecopening("GA-A02", "grandabalone", "Grand Belgian Daisy", "Vanguard", "a1d4 b7b4", "SS2ss/SSS1sss/1SS2ss1/9/ss6SS/sss5SSS/ss6SS/3S5/1SS2ss1/SSSsss1/1S2ss", "b7b4"),
"GA-A03" -> new Ecopening("GA-A03", "grandabalone", "Grand Belgian Daisy", "Double Vanguard", "a1d4 b7b4 a5d8", "SS2ss/SSS1sss/1SS2ss1/9/ss6SS/sss5SSS/ss6SS/3S3s1/1SS2ss1/SSSsss1/1S3s", "a5d8"),
"GA-B01" -> new Ecopening("GA-B01", "grandabalone", "Grand Belgian Daisy", "Scorpio - B", "k6h6", "1S2ss/SSS1sss/1SS2ss1/3S5/ss6SS/sss5SSS/ss6SS/9/1SS2ss1/SSS1sss/SS2ss", "k6h6"),
"GA-B02" -> new Ecopening("GA-B02", "grandabalone", "Grand Belgian Daisy", "Vanguard - B", "k6h6 e1h4", "1S2ss/SSS1sss/1SS2ss1/1s1S5/ss6SS/sss5SSS/1s6SS/9/1SS2ss1/SSS1sss/SS2ss", "e1h4"),
"GA-B03" -> new Ecopening("GA-B03", "grandabalone", "Grand Belgian Daisy", "Double Vanguard - B", "k6h6 e1h4 g2d2", "1S2ss/SSS1sss/1SS2ss1/1s1S5/1s6SS/sss5SSS/1s6SS/1s7/1SS2ss1/SSS1sss/SS2ss", "g2d2"),
"GA-C01" -> new Ecopening("GA-C01", "grandabalone", "Grand Belgian Daisy", "Scorpio - C", "f11f8", "SS2ss/SSS1sss/1SS2ss1/9/ss6SS/sss4SSS1/ss6SS/9/1SS2ss1/SSS1sss/SS2ss", "f11f8"),
"GA-C02" -> new Ecopening("GA-C02", "grandabalone", "Grand Belgian Daisy", "Vanguard - C", "f11f8 k10h10", "SS3s/SSS1sss/1SS2ss1/7s1/ss6SS/sss4SSS1/ss6SS/9/1SS2ss1/SSS1sss/SS2ss", "k10h10"),
"GA-C03" -> new Ecopening("GA-C03", "grandabalone", "Grand Belgian Daisy", "Double Vanguard - C", "f11f8 k10h10 j11j8", "SS3s/SSSsss1/1SS2ss1/7s1/ss6SS/sss4SSS1/ss6SS/9/1SS2ss1/SSS1sss/SS2ss", "j11j8"),
  )
}
