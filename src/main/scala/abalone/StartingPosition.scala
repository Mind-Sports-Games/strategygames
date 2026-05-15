package strategygames.abalone

import strategygames.abalone.format.FEN

case class StartingPosition(
    eco: String,
    name: String,
    fen: FEN,
    wikiPath: String,
    moves: String,
    featurable: Boolean = true
)

object StartingPosition {
  case class Category(name: String, variantGrouping: String, positions: List[StartingPosition])

  val categories: List[Category] = List(
    Category("Abalone", "abalone", List(
      StartingPosition("X00", "Standard", FEN("sssss/ssssss/2sss2/8/9/8/2SSS2/SSSSSS/SSSSS 0 0 b 0 1"), "", ""),
      StartingPosition("X01", "Snakes", FEN("SSSSS/S5/S6/S1sssss1/1S5s1/1SSSSS1s/6s/5s/sssss 0 0 b 0 0"), "", ""),
      StartingPosition("X02", "Alien Attack", FEN("S1S1S/1SssS1/1SsSsS1/3SS3/9/3ss3/1sSsSs1/1sSSs1/s1s1s 0 0 b 0 1"), "", ""),
      StartingPosition("X03", "Checkerboard", FEN("5/SsSsSs/7/sSsSsSsS/9/SsSsSsSs/7/sSsSsS/5 0 0 b 0 1"), "", ""),
      StartingPosition("X04", "Duel", FEN("1s1S1/sssSSS/2s1S2/S6s/SS5ss/S6s/2s1S2/sssSSS/1s1S1 0 0 b 0 1"), "", ""),
      StartingPosition("X05", "Dutch Daisy", FEN("ss1SS/sSsSsS/1ss1SS1/8/9/8/1SS1ss1/SsSsSs/SS1ss 0 0 b 0 1"), "", ""),
      StartingPosition("X06", "German Daisy", FEN("5/ss2SS/sss1SSS/1ss2SS1/9/1SS2ss1/SSS1sss/SS2ss/5 0 0 b 0 1"), "", ""),
      StartingPosition("X07", "Swiss Daisy", FEN("5/ss2SS/sSs1SsS/1ss2SS1/9/1SS2ss1/SsS1sSs/SS2ss/5 0 0 b 0 1"), "", ""),
      StartingPosition("X08", "Star", FEN("S3s/1s2S1/s1S1s1S/3sS3/sSsS1sSsS/3sS3/s1S1s1S/1s2S1/S3s 0 0 b 0 1"), "", ""),
      StartingPosition("X09", "The wall", FEN("2s2/6/1sssss1/ssssssss/9/SSSSSSSS/1SSSSS1/6/2S2 0 0 b 0 1"), "", ""),
      StartingPosition("X10", "The Clearing", FEN("SsSsS/s4s/1S2s1S/Ss5s/s7S/S5Ss/s1S2s1/S4S/sSsSs 0 0 b 0 1"), "", ""),
      StartingPosition("X11", "Papillons", FEN("1S1ss/S1sS1s/1s3SS/sS5s/s7S/S5sS/ss3S1/S1sS1s/SS1s1 0 0 b 0 1"), "", ""),
    ))
  )

  val all: IndexedSeq[StartingPosition] = categories.flatMap(_.positions).toIndexedSeq

  def forVariant(variantGrouping: String): IndexedSeq[StartingPosition] =
    categories.filter(_.variantGrouping == variantGrouping).flatMap(_.positions).toIndexedSeq

  val initial = StartingPosition("---", "Belgian Daisy", format.Forsyth.initial, "", "")

  def allWithInitial = initial +: all
}
