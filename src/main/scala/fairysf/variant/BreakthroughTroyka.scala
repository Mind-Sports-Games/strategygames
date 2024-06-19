package strategygames.fairysf
package variant

import strategygames.GameFamily

case object BreakthroughTroyka
    extends Variant(
      id = 9, // unique within module fairySF
      key = "breakthroughtroyka",
      name = "Breakthrough",
      standardInitialPosition = true,
      fairysfName = FairySFName("breakthrough"),
      boardSize = Board.Dim8x8
    ) {

  def gameFamily: GameFamily = GameFamily.BreakthroughTroyka()

  def perfIcon: Char                      = 'a' // @TODO: adapt
  def perfId: Int                         = 208 // just need to be unique accross all variants
  override def canOfferDraw               = false
  override def baseVariant: Boolean       = true
  override def p1IsBetterVariant: Boolean = true

  override def initialFen =
    format.FEN("pppppppp/pppppppp/8/8/8/8/PPPPPPPP/PPPPPPPP w - - 0 1")

  override def checkmate(situation: Situation) = false

  override def staleMate(situation: Situation) = false

  // The game ends with a loss if you run out of piece or if opponent reaches opposite final rank
  override def specialEnd(situation: Situation): Boolean =
    (situation.board.pieces.filter(e => e._2.player.p2).isEmpty
      || situation.board.pieces.filter(e => e._2.player.p1).isEmpty
      || (situation.board.pieces exists {
        case (pos, piece) => {
          (pos.rank == Rank.Eighth && piece.player == P1) || (pos.rank == Rank.First && piece.player == P2)
        }
      }))
}
