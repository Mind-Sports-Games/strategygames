package strategygames.chess.variant

import strategygames.chess._
import strategygames.chess.format.FEN
import strategygames.Player

case object Monster
    extends Variant(
      id = 15,
      key = "monster",
      name = "Monster",
      standardInitialPosition = false
    ) {

  def perfId: Int    = 23
  def perfIcon: Char = ''

  override def hasAnalysisBoard: Boolean = false
  override def hasFishnet: Boolean       = false

  override def exoticChessVariant       = true
  // override def p1IsBetterVariant        = true
  override def blindModeVariant         = false
  override def materialImbalanceVariant = true

  lazy val pieces: Map[Pos, Piece] = {

    val p1Pieces = Map(
      Pos.E1 -> Piece(P1, King),
      Pos.C2 -> Piece(P2, Pawn),
      Pos.D2 -> Piece(P2, Pawn),
      Pos.E2 -> Piece(P2, Pawn),
      Pos.F2 -> Piece(P2, Pawn)
    )

    val p2Pieces = (for (y <- List(Rank.Seventh, Rank.Eighth); x <- File.all) yield {
      Pos(x, y) -> (y match {
        case Rank.Eighth  => Piece(P2, backRank(x.index))
        case Rank.Seventh => Piece(P2, Pawn)
      })
    }).toMap

    p1Pieces ++ p2Pieces
  }

  override val castles = Castles("kq")

  override val initialFen = FEN("rnbqkbnr/pppppppp/8/8/8/8/2PPPP2/4K3 w kq - 0 1")

  // override def valid(board: Board, strict: Boolean) =

}
