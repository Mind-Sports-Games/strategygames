package strategygames.fairysf
package variant

import strategygames.GameFamily
import strategygames.Player

case object MiniBreakthroughTroyka
    extends Variant(
      id = 10,
      key = "minibreakthroughtroyka",
      name = "Mini Breakthrough",
      standardInitialPosition = true,
      fairysfName = FairySFName("ps-minibreakthrough"),
      boardSize = Board.Dim5x5
    ) {

  def gameFamily: GameFamily = GameFamily.Breakthrough()

  def perfIcon: Char                      = 'a'
  def perfId: Int                         = 209
  override def canOfferDraw               = false
  override def repetitionEnabled: Boolean = true
  override def p1IsBetterVariant: Boolean = true

  override def startPlayer: Player = P1

  override def initialFen =
    format.FEN("ppppp/ppppp/5/PPPPP/PPPPP w - - 0 1")

  override def checkmate(situation: Situation) = false

  override def staleMate(situation: Situation) = false

  override def specialEnd(situation: Situation): Boolean =
    (situation.board.pieces.filter(e => e._2.player.p2).isEmpty
      || situation.board.pieces.filter(e => e._2.player.p1).isEmpty
      || (situation.board.pieces exists {
        case (pos, piece) => {
          (pos.rank == Rank.Fifth && piece.player == P1) || (pos.rank == Rank.First && piece.player == P2)
        }
      }))
}
