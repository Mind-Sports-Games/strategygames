package strategygames.chess.variant

import strategygames.chess._
import strategygames.chess.format.FEN
import strategygames.Player

case object RacingKings
    extends Variant(
      id = 9,
      key = "racingKings",
      name = "Racing Kings",
      standardInitialPosition = false
    ) {

  def perfId: Int    = 17
  def perfIcon: Char = ''

  override def p1IsBetterVariant = true
  override def blindModeVariant  = false

  override def allowsCastling = false

  // Both sides start on the first two ranks:
  // krbnNBRK
  // qrbnNBRQ
  override val pieces: Map[Pos, Piece] = Map(
    Pos.A1 -> Piece(P2, Queen),
    Pos.A2 -> Piece(P2, King),
    Pos.B1 -> Piece(P2, Rook),
    Pos.B2 -> Piece(P2, Rook),
    Pos.C1 -> Piece(P2, Bishop),
    Pos.C2 -> Piece(P2, Bishop),
    Pos.D1 -> Piece(P2, Knight),
    Pos.D2 -> Piece(P2, Knight),
    Pos.E1 -> Piece(P1, Knight),
    Pos.E2 -> Piece(P1, Knight),
    Pos.F1 -> Piece(P1, Bishop),
    Pos.F2 -> Piece(P1, Bishop),
    Pos.G1 -> Piece(P1, Rook),
    Pos.G2 -> Piece(P1, Rook),
    Pos.H1 -> Piece(P1, Queen),
    Pos.H2 -> Piece(P1, King)
  )

  override val castles = Castles.none

  override val initialFen = FEN("8/8/8/8/8/8/krbnNBRK/qrbnNBRQ w - - 0 1")

  override def isInsufficientMaterial(board: Board)                  = false
  override def opponentHasInsufficientMaterial(situation: Situation) = false

  private def reachedGoal(board: Board, player: Player) =
    board.kingPosOf(player) exists (_.rank == Rank.Eighth)

  private def reachesGoal(move: Move) =
    reachedGoal(move.situationAfter.board, move.piece.player)

  // It is a win, when exactly one king made it to the goal. When p1 reaches
  // the goal and p2 can make it on the next ply, he is given a chance to
  // draw, to compensate for the first-move advantage. The draw is not called
  // automatically, because p2 should also be given equal chances to flag.
  override def specialEnd(situation: Situation) =
    situation.player match {
      case P1 =>
        reachedGoal(situation.board, P1) ^ reachedGoal(situation.board, P2)
      case P2 =>
        reachedGoal(situation.board, P1) && (validMoves(situation).view mapValues (_.filter(reachesGoal)))
          .forall(_._2.isEmpty)
    }

  // If p1 reaches the goal and p2 also reaches the goal directly after,
  // then it is a draw.
  override def specialDraw(situation: Situation) =
    situation.player.p1 && reachedGoal(situation.board, P1) && reachedGoal(situation.board, P2)

  override def winner(situation: Situation): Option[Player] =
    specialEnd(situation) option Player.fromP1(reachedGoal(situation.board, P1))

  // Not only check that our king is safe,
  // but also check the opponent's
  override def kingSafety(m: Move, filter: Piece => Boolean, kingPos: Option[Pos]): Boolean =
    super.kingSafety(m, filter, kingPos) && ! {
      m.after.kingPos get !m.player exists { theirKingPos =>
        kingThreatened(m.after, m.player, theirKingPos, (_ => true))
      }
    }

  // When considering stalemate, take into account that checks are not allowed.
  override def staleMate(situation: Situation): Boolean =
    !situation.check && !specialEnd(situation) && !validMoves(situation).exists(_._2.nonEmpty)
}
