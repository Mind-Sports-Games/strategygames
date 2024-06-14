package strategygames.fairysf
package variant

import strategygames.GameFamily
import strategygames.Player

import cats.implicits._

case object MiniBreakthroughTroyka
    extends Variant(
      id = 10,
      key = "minibreakthroughtroyka",
      name = "Mini Breakthrough",
      standardInitialPosition = false,
      fairysfName = FairySFName("ps-minibreakthrough"),
      boardSize = Board.Dim5x5
    ) {

  def gameFamily: GameFamily = GameFamily.Breakthrough()

  def perfIcon: Char                      = 'a'
  def perfId: Int                         = 209
  override def canOfferDraw               = false
  override def baseVariant: Boolean       = true
  override def repetitionEnabled: Boolean = false
  override def useFairyOptionalGameEnd    = true
  override def p1IsBetterVariant: Boolean = true

  override def startPlayer: Player = P1

  // cache this rather than checking with the API everytime
  override def initialFen =
    format.FEN("ppppp/ppppp/5/PPPPP/PPPPP w - - 0 1")

  override def staleMate(situation: Situation): Boolean = situation.board.apiPosition.legalMoves.isEmpty

  override val pieces: Map[Pos, Piece] = Map(
    Pos.A1 -> Piece(P1, BreakthroughPawn),
    Pos.B1 -> Piece(P1, BreakthroughPawn),
    Pos.C1 -> Piece(P1, BreakthroughPawn),
    Pos.D1 -> Piece(P1, BreakthroughPawn),
    Pos.E1 -> Piece(P1, BreakthroughPawn),
    Pos.A2 -> Piece(P1, BreakthroughPawn),
    Pos.B2 -> Piece(P1, BreakthroughPawn),
    Pos.C2 -> Piece(P1, BreakthroughPawn),
    Pos.D2 -> Piece(P1, BreakthroughPawn),
    Pos.E2 -> Piece(P1, BreakthroughPawn),
    Pos.A5 -> Piece(P2, BreakthroughPawn),
    Pos.B5 -> Piece(P2, BreakthroughPawn),
    Pos.C5 -> Piece(P2, BreakthroughPawn),
    Pos.D5 -> Piece(P2, BreakthroughPawn),
    Pos.E5 -> Piece(P2, BreakthroughPawn),
    Pos.A4 -> Piece(P2, BreakthroughPawn),
    Pos.B4 -> Piece(P2, BreakthroughPawn),
    Pos.C4 -> Piece(P2, BreakthroughPawn),
    Pos.D4 -> Piece(P2, BreakthroughPawn),
    Pos.E4 -> Piece(P2, BreakthroughPawn)
  )

  override def specialEnd(situation: Situation): Boolean =
    reachedGoal(situation.board)

  def reachedGoal(board: Board): Boolean =
    board.pieces exists {
      case (pos, piece) => {
        (pos.rank == Rank.Fifth && piece.player == P1) || (pos.rank == Rank.First && piece.player == P2)
      }
    }

  override def winner(situation: Situation): Option[Player] = {
    if (situation.staleMate) Some(!situation.player)
    else if (reachedGoal(situation.board)) {
      if (situation.player.p1)
        P2.some // @TODO: check if we have to swap or not
      else
        P1.some
    }
    None
  }

}
