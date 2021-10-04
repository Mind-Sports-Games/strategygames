package strategygames.chess.variant

import strategygames.chess._
import strategygames.chess.format.FEN
import strategygames.Color

case object RacingKings
    extends Variant(
      id = 9,
      key = "racingKings",
      name = "Racing Kings",
      shortName = "Racing",
      title = "Race your King to the eighth rank to win.",
      standardInitialPosition = false
    ) {

  override def whiteIsBetterVariant = true
  override def blindModeVariant = false

  override def allowsCastling = false

  // Both sides start on the first two ranks:
  // krbnNBRK
  // qrbnNBRQ
  override val pieces: Map[Pos, Piece] = Map(
    Pos.A1 -> Piece(Black, Queen),
    Pos.A2 -> Piece(Black, King),
    Pos.B1 -> Piece(Black, Rook),
    Pos.B2 -> Piece(Black, Rook),
    Pos.C1 -> Piece(Black, Bishop),
    Pos.C2 -> Piece(Black, Bishop),
    Pos.D1 -> Piece(Black, Knight),
    Pos.D2 -> Piece(Black, Knight),
    Pos.E1 -> Piece(White, Knight),
    Pos.E2 -> Piece(White, Knight),
    Pos.F1 -> Piece(White, Bishop),
    Pos.F2 -> Piece(White, Bishop),
    Pos.G1 -> Piece(White, Rook),
    Pos.G2 -> Piece(White, Rook),
    Pos.H1 -> Piece(White, Queen),
    Pos.H2 -> Piece(White, King)
  )

  override val castles = Castles.none

  override val initialFen = FEN("8/8/8/8/8/8/krbnNBRK/qrbnNBRQ w - - 0 1")

  override def isInsufficientMaterial(board: Board)                  = false
  override def opponentHasInsufficientMaterial(situation: Situation) = false

  private def reachedGoal(board: Board, color: Color) =
    board.kingPosOf(color) exists (_.rank == Rank.Eighth)

  private def reachesGoal(move: Move) =
    reachedGoal(move.situationAfter.board, move.piece.color)

  // It is a win, when exactly one king made it to the goal. When white reaches
  // the goal and black can make it on the next ply, he is given a chance to
  // draw, to compensate for the first-move advantage. The draw is not called
  // automatically, because black should also be given equal chances to flag.
  override def specialEnd(situation: Situation) =
    situation.color match {
      case White =>
        reachedGoal(situation.board, White) ^ reachedGoal(situation.board, Black)
      case Black =>
        reachedGoal(situation.board, White) && (validMoves(situation).view mapValues (_.filter(reachesGoal)))
          .forall(_._2.isEmpty)
    }

  // If white reaches the goal and black also reaches the goal directly after,
  // then it is a draw.
  override def specialDraw(situation: Situation) =
    situation.color.white && reachedGoal(situation.board, White) && reachedGoal(situation.board, Black)

  override def winner(situation: Situation): Option[Color] =
    specialEnd(situation) option Color.fromWhite(reachedGoal(situation.board, White))

  // Not only check that our king is safe,
  // but also check the opponent's
  override def kingSafety(m: Move, filter: Piece => Boolean, kingPos: Option[Pos]): Boolean =
    super.kingSafety(m, filter, kingPos) && ! {
      m.after.kingPos get !m.color exists { theirKingPos =>
        kingThreatened(m.after, m.color, theirKingPos, (_ => true))
      }
    }

  // When considering stalemate, take into account that checks are not allowed.
  override def staleMate(situation: Situation): Boolean =
    !situation.check && !specialEnd(situation) && !validMoves(situation).exists(_._2.nonEmpty)
}
