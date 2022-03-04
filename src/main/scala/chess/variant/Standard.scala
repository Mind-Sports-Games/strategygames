package strategygames.chess.variant
import strategygames.chess._

case object Standard
    extends Variant(
      id = 1,
      key = "standard",
      name = "Standard Chess",
      standardInitialPosition = true
    ) {

  def perfId: Int    = 5
  def perfIcon: Char = '8'

  val pieces: Map[Pos, Piece] = Variant.symmetricRank(backRank)

  override def baseVariant: Boolean = true
}
