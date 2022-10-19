package strategygames.chess.variant
import strategygames.chess._
import strategygames.chess.format.FEN

case object NoCastling
    extends Variant(
      id = 13,
      key = "noCastling",
      name = "No Castling",
      standardInitialPosition = true
    ) {

  def perfId: Int    = 20
  def perfIcon: Char = ''

  override val castles    = Castles.none
  override val initialFen = FEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1")

  val pieces: Map[Pos, Piece] = Variant.symmetricRank(backRank)

  override def baseVariant: Boolean = true
}
